;;; lb-datalog-mode.el --- Major mode for editing Datalog code

;; Copyright (C) 2014, George Balatsouras
;;
;; Author: George Balatsouras <gbalats(at)gmail(dot)com>
;; Maintainer: George Balatsouras <gbalats(at)gmail(dot)com>
;; Created: 26 Aug 2014
;; Version: 0.2
;; Keywords: convenience, languages
;;
;; This file is NOT part of Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA


;;; Commentary:

;; This is a major mode for the LogicBlox Datalog language.  It
;; provides syntax highlighting, movement functions, and expansions.
;;
;; Put this file into your load-path and the following into your
;; `~/.emacs':
;;
;; (require 'lb-datalog-mode)


;;; Code:

;;----------------------------
;; Prerequisites
;;----------------------------

(require 'dash)
(require 'f)
(require 'font-lock)
(require 'multiple-cursors)
(require 'newcomment)
(require 's)
(require 'smie)
(eval-when-compile
  (require 'compile)
  (require 'regexp-opt))


(require 'lb-datalog-core)
(require 'lb-datalog-compile)
(require 'lb-datalog-project)
(require 'lb-datalog-connect)

;;----------------------------
;; Local variables
;;----------------------------

(defcustom lb-datalog-default-face 'default
  "Default face in `lb-datalog-mode' buffers."
  :type  'face
  :group 'lb-datalog)

(defcustom lb-datalog-predicate-face 'font-lock-function-name-face
  "Face for predicate names in `lb-datalog-mode' buffers."
  :type  'face
  :group 'lb-datalog)

(defcustom lb-datalog-mode-hook nil
  "*List of functions to be executed on entry to `lb-datalog-mode'.*"
  :type  'hook
  :group 'lb-datalog)

(defcustom lb-datalog-indent-width 3
  "Level of indentation in `lb-datalog-mode' buffers."
  :type 'integer
  :group 'lb-datalog)

(defvar lb-datalog-electric-newline-p t
  "*Non-nil means automatically indent the next line when the user types RET.")



;;----------------------------
;; Keywords
;;----------------------------

(defconst lb-datalog-keywords-regexp
  (eval-when-compile
    (regexp-opt '("not" "exists" "true" "false") 'symbols))
  "Regular expression for LB Datalog keywords.")

(defconst lb-datalog-types-regexp
  (eval-when-compile
    (regexp-opt '("int" "int[8]" "int[16]" "int[32]" "int[64]"
                  "uint[8]" "uint[16]" "uint[32]" "uint[64]"
                  "float" "float[32]" "float[64]"
                  "decimal" "decimal[64]" "decimal[128]"
                  "boolean" "string" "datetime" "color")
                'symbols))
  "Regular expression for LB Datalog types.")

(defconst lb-datalog-number-regexp
  (concat "\\<[[:digit:]]+"
          "\\(?:\\.[[:digit:]]+\\)?"
          "\\(?:[eE][+-]?[[:digit:]]+\\)?\\>")
  "Regular expression for LB Datalog numbers.")

(defconst lb-datalog-aggregation-regexp
  (concat "\\(agg\\)\\s-*<<.*>>")
  "Regular expression for LB Datalog aggregations.")

(defconst lb-datalog-stage-modifier-regexp
  (concat "@\\(?:"
          "prev\\(?:ious\\)?"
          "\\|"
          "final"
          "\\|"
          "init\\(?:ial\\)?"
          "\\)")
  "Regular expression for LB Datalog stage modifiers.")

(defvar lb-datalog-font-lock-keywords
  (let* ((variable-regexp "[_?[:alpha:]][_[:word:]]*")
         (predicate-name-regexp "[_[:alpha:]][_:?$[:word:]]*")
         (predicate-ref-regexp (concat "`" predicate-name-regexp))
         (builtin-predicate-regexp (concat "\\_<lang:" predicate-name-regexp))
         (predicate-regexp
          (concat "\\(" predicate-name-regexp "\\)"
                  "\\s(.*?\\s)"))       ; non-greedy *? operator inside paren
         (staged-predicate-regexp
          (concat "\\(" predicate-name-regexp "\\)"
                  "\\(" lb-datalog-stage-modifier-regexp "\\)"
                  "\\s(.*?\\s)")))      ; non-greedy *? operator inside paren
    `((,lb-datalog-types-regexp . font-lock-type-face)
      (,lb-datalog-keywords-regexp . font-lock-keyword-face)
      (,lb-datalog-aggregation-regexp 1 font-lock-keyword-face)
      (,predicate-ref-regexp . font-lock-reference-face)
      (,builtin-predicate-regexp . font-lock-builtin-face)
      (,staged-predicate-regexp (1 lb-datalog-predicate-face)
                                (2 font-lock-builtin-face))
      (,predicate-regexp 1 lb-datalog-predicate-face)
      (,lb-datalog-number-regexp . font-lock-warning-face)
      (,variable-regexp . font-lock-variable-name-face)))
  "Font-lock keywords for `lb-datalog-mode'.")


;;----------------------------
;; Indentation
;;----------------------------

;; (defvar lb-datalog-smie-grammar
;;   (smie-prec2->grammar
;;    (smie-bnf->prec2
;;     '((id)
;;       (clause (atoms "<-" atoms ".")
;;               (atoms "<-" "agg" "<<" exp "=" atom ">>" atoms ".")
;;               (atoms "->" atoms ".")
;;               (atoms "."))
;;       (atom (pred "(" exps ")")
;;             (pred "[" exps "]" "=" exp))
;;       (atoms (atoms "," atoms)
;;              (atoms ";" atoms)
;;              ("!" atom)
;;              (atom))
;;       (exp (id)
;;            (exp "+" exp)
;;            (exp "-" exp)
;;            (exp "*" exp)
;;            (exp "/" exp))
;;       (exps (exps "," exps) (exp)))
;;     '((assoc ",") (assoc ";"))
;;     '((assoc "+") (assoc "-") (assoc "*") (assoc "/")))))

(defconst lb-datalog-smie-grammar
  ;; Rather than construct the operator levels table from the BNF, we
  ;; directly provide the operator precedences as in GNU Prolog's
  ;; manual. The only problem is that GNU Prolog's manual uses
  ;; precedence levels in the opposite sense (higher numbers bind less
  ;; tightly) than SMIE, so we use negative numbers.
  '(("." -10000 -10000)
    ("<-" -1200 -1200)
    ("->" -1200 -1200)
    (";" -1100 -1100)
    ("," -1000 -1000)
    ("=" -700 -700)
    ("<" -700 -700)
    ("<=" -700 -700)
    (">" -700 -700)
    (">=" -700 -700)
    ("+" -500 -500)
    ("-" -500 -500)
    ("*" -400 -400)
    ("/" -400 -400)
    (:smie-closer-alist (t . ".")))
  "Precedence levels of infix operators.")

(defun lb-datalog-smie-rules (kind token)
  (pcase (cons kind token)
    (`(:elem . basic) lb-datalog-indent-width)
    (`(:after . ".") '(column . 0)) ; To work around smie-closer-alist.
    (`(:before . ",")
     (when (smie-rule-bolp)
       (cond ((smie-rule-parent-p "<-" "->") (smie-rule-parent 0))
             ((not (smie-rule-sibling-p)) (smie-rule-parent 1)))))
    (`(,_ . ,(or `"," `";")) (smie-rule-separator kind))
    (`(:before . ,(or `"<-" `"->"))
     (when (smie-rule-bolp) (smie-rule-parent 1)))
    (`(:after . ,(or `"<-" `"->"))
     (if (smie-rule-hanging-p) lb-datalog-indent-width
       (1- lb-datalog-indent-width)))))


;;----------------------------
;; Comment-specific commands
;;----------------------------

(defun lb-datalog-comment-dwim (arg)
  "Comment or uncomment current line or region in a smart way.
For detail, see `comment-dwim'."
  (interactive "*P")
  (let ((comment-start "//") (comment-end ""))
    (comment-dwim arg)))


;;----------------------------
;; refactorings
;;----------------------------

(defun lb-datalog-narrow-to-clause ()
  (save-excursion
    (lb-datalog-forward-clause 1)       ; move to clause ending
    (set-mark (point))
    (lb-datalog-backward-clause 1)      ; move to clause beginning
    (deactivate-mark)
    (narrow-to-region (point) (mark))))

;;;###autoload
(defun lb-datalog-rename-symbol ()
  (interactive)
  (save-restriction
    (lb-datalog-narrow-to-clause)
    (mc/mark-all-symbols-like-this)))

(add-to-list 'mc--default-cmds-to-run-once
             'lb-datalog-rename-symbol)


;;----------------------------
;; syntax table
;;----------------------------

(defvar lb-datalog-syntax-table
  (let ((st (make-syntax-table)))
    ;; C++ style comment `//' ..."
    (modify-syntax-entry ?/ ". 124b" st)
    (modify-syntax-entry ?* ". 23" st)
    (modify-syntax-entry ?\n "> b" st)
    (modify-syntax-entry ?: "_" st)
    (modify-syntax-entry ?? "_" st)
    st)
  "Syntax table for `lb-datalog-mode'.")


;;----------------------------
;; keymap
;;----------------------------

(defvar lb-datalog-mode-map
  (let ((map (make-sparse-keymap)))
    ;; modify the keymap
    (define-key map (kbd "C-c :") 'lb-datalog-rename-symbol)
    (define-key map (kbd "C-M-e") 'lb-datalog-forward-clause)
    (define-key map (kbd "C-M-a") 'lb-datalog-backward-clause)
    (define-key map (kbd "M-e") 'lb-datalog-forward-atom)
    (define-key map (kbd "M-a") 'lb-datalog-backward-atom)
    (define-key map [remap comment-dwim] 'lb-datalog-comment-dwim)
    (when lb-datalog-electric-newline-p
      (define-key map "\r" 'reindent-then-newline-and-indent))
    map)
  "Keymap for `lb-datalog-mode'.")


;;----------------------------
;; backwards compatibility
;;----------------------------

(when (< emacs-major-version 24)
  (defalias 'prog-mode 'fundamental-mode))


;;----------------------------
;; define the mode
;;----------------------------

;;;###autoload
(define-derived-mode lb-datalog-mode prog-mode "lb-datalog mode"
  "Major mode for editing LB Datalog ..."
  :group 'lb-datalog

  (kill-all-local-variables)

  ;; Select the mode's keymap.
  (use-local-map lb-datalog-mode-map)

  ;; Comments start with `//'.
  (set (make-local-variable 'comment-start) "//")

  ;; code for syntax highlighting
  (set (make-local-variable 'font-lock-defaults)
       '(lb-datalog-font-lock-keywords))

  ;; syntax table
  (set-syntax-table lb-datalog-syntax-table)

  ;; smie setup
  (smie-setup lb-datalog-smie-grammar #'lb-datalog-smie-rules)

  ;; major mode name
  (setq mode-name "LB-Datalog")
  (setq major-mode 'lb-datalog-mode)

  ;; permit the user to customize the mode with a hook
  (run-mode-hooks 'lb-datalog-mode-hook))


;;----------------------------
;; Add file association
;;----------------------------

(add-to-list 'auto-mode-alist '("\\.logic$" . lb-datalog-mode))


(provide 'lb-datalog-mode)

;;; lb-datalog-mode.el ends here
