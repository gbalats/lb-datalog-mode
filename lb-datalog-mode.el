;;; lb-datalog-mode.el --- major mode for editing Datalog code

;; Author: George Balatsouras
;; Keywords: LogicBlox Datalog


;;; Commentary:

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'lb-datalog-mode)


;;; Code:

(provide 'lb-datalog-mode)

;; Prerequisites
(require 'font-lock)
(require 'newcomment)
(eval-when-compile
  (require 'regexp-opt))

;; Local variables
(defgroup lb-datalog nil
  "Major mode `lb-datalog-mode' for editing LB Datalog code."
  :prefix "lb-datalog-"
  :group  'languages)

(defcustom lb-datalog-default-face 'default
  "Default face in `lb-datalog-mode' buffers."
  :type  'face
  :group 'lb-datalog)

(defcustom lb-datalog-predicate-face 'font-lock-builtin-face
  "Face for predicate names in `lb-datalog-mode' buffers."
  :type  'face
  :group 'lb-datalog)

(defcustom lb-datalog-mode-hook nil
  "List of functions to be executed on entry to `lb-datalog-mode'."
  :type  'hook
  :group 'lb-datalog)

(defconst lb-datalog-keywords
      '("agg" "not" "exists" "true" "false"))

(defconst lb-datalog-types
      '("int" "int[8]" "int[16]" "int[32]" "int[64]"
        "uint[8]" "uint[16]" "uint[32]" "uint[64]"
        "float" "float[32]" "float[64]"
        "decimal" "decimal[64]" "decimal[128]"
        "boolean" "string" "datetime" "color"))

(defconst lb-datalog-keywords-regexp
  (eval-when-compile
    (regexp-opt lb-datalog-keywords 'words)))

(defconst lb-datalog-types-regexp
  (eval-when-compile
    (regexp-opt lb-datalog-types 'words)))

(defconst lb-datalog-number-regexp
  (concat "\\<[[:digit:]]+"
          "\\(?:\\.[[:digit:]]+\\)?"
          "\\(?:[eE][+-]?[[:digit:]]+\\)?\\>")
  "Regular expression for LB Datalog numbers.")

(defvar lb-datalog-font-lock-keywords
  (let* ((variable-regexp "[[:alpha:]_?][[:word:]_]*")
         (predicate-name-regexp "\\sw+\\(?:[:_?$]\\sw+\\)*")
         (predicate-ref-regexp (concat "`" predicate-name-regexp))
         (predicate-regexp (concat "\\(" predicate-name-regexp "\\)"
                                   "\\s(.*?\\s)")))
    `((,lb-datalog-types-regexp . font-lock-type-face)
      (,lb-datalog-keywords-regexp . font-lock-keyword-face)
      (,lb-datalog-number-regexp . font-lock-warning-face)
      (,predicate-ref-regexp . font-lock-reference-face)
      (,predicate-regexp 1 lb-datalog-predicate-face)
      (,variable-regexp . font-lock-variable-name-face)))
  "Font-lock keywords for `lb-datalog-mode'.")

;; command to comment/uncomment text
(defun lb-datalog-comment-dwim (arg)
  "Comment or uncomment current line or region in a smart way.
For detail, see `comment-dwim'."
  (interactive "*P")
  (let ((comment-start "//") (comment-end ""))
    (comment-dwim arg)))

;; syntax table
(defvar lb-datalog-syntax-table
  (let ((st (make-syntax-table)))
    ;; C++ style comment `//' ..."
    (modify-syntax-entry ?/ ". 124b" st)
    (modify-syntax-entry ?* ". 23" st)
    (modify-syntax-entry ?\n "> b" st)
    st)
  "Syntax table for `lb-datalog-mode'.")


;; define the mode
(define-derived-mode lb-datalog-mode fundamental-mode "lb-datalog mode"
  "Major mode for editing LB Datalog ..."

  ;; Comments start with `//'.
  (set (make-local-variable 'comment-start) "//")

  ;; code for syntax highlighting
  (set (make-local-variable 'font-lock-defaults)
       '(lb-datalog-font-lock-keywords))

  ;; mode name
  (set (make-local-variable 'mode-name) "lb-datalog")

  ;; syntax table
  (set-syntax-table lb-datalog-syntax-table)

  ;; modify the keymap
  (define-key lb-datalog-mode-map [remap comment-dwim]
    'lb-datalog-comment-dwim))


;; Add file association
(add-to-list 'auto-mode-alist '("\\.logic$" . lb-datalog-mode))
