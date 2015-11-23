;;; lb-datalog-compile.el -- LB Datalog compilation buffer support

;; Copyright (C) 2014, George Balatsouras
;;
;; Author: George Balatsouras <gbalats(at)gmail(dot)com>
;; Keywords: convenience, languages
;;
;; This file is NOT part of Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
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

;; This file provides better support for the compilation buffer.
;; Namely, it defines several error message formats that can be
;; returned during compilation, and adds them to
;; `compilation-error-regexp-alist'.

;; Please see lb-datalog-mode.el for more commentary.

;;; Code:

(require 'f)
(require 's)
(eval-when-compile
  (require 'compile))

(defconst lb-datalog-compiler "bloxcompiler"
  "The compiler command for LB Datalog files and projects.")

(defun lb-datalog--block-regexp-to-filename ()
  "Return the filename of the error in the compilation buffer.
This corresponds to the regular expression which captures the block name."
  (let ((block-regexp (match-string 1))
        (command compile-command))
    ;; Search backwards for bloxcompiler line. This is helpful if the
    ;; compilation command is invoking another tool that calls
    ;; bloxcompiler in turn.
    (if (not (s-starts-with? lb-datalog-compiler command))
        (save-match-data
          (save-excursion
            (if (word-search-backward "bloxcompiler" nil t)
                (setq command (thing-at-point 'line))))))
    (let* ((project-file
            (car (s-match "\\<[^[[:space:]]*\\.project\\>" command))))
      (f-join compilation-directory
              (concat block-regexp ".logic")))))

(defvar lb-compilation-error-regexp-alist1
  (eval-when-compile
    `(,(concat "^"
               "File \\(.+\\) (Block .+) : "
               "Line \\([0-9]+\\), "
               "Columns \\([0-9]+\\)-\\([0-9]+\\):"
               "$")
      (1 "%s.logic") 2 (3 . 4) nil 1))
  "Compilation error regexp-alist for `lb-datalog-mode' buffers.")

(defvar lb-compilation-error-regexp-alist2
  (eval-when-compile
    `(,(concat "^"
               "File \\(.+\\) (Block .+) : "
               "Line \\([0-9]+\\), "
               "Column \\([0-9]+\\):"
               ".*$")
      (1 "%s.logic") 2 3 nil 1))
  "Compilation error regexp-alist for `lb-datalog-mode' buffers.")

(defvar lb-compilation-error-regexp-alist3
  (eval-when-compile
    `(,(concat "^"
               "Block \\(.+\\): "
               "Line \\([0-9]+\\), "
               "Columns \\([0-9]+\\)-\\([0-9]+\\):"
               ".*$")
      lb-datalog--block-regexp-to-filename 2 (3 . 4) nil 1))
  "Compilation error regexp-alist for `lb-datalog-mode' buffers.")

(defvar lb-compilation-error-regexp-alist4
  (eval-when-compile
    `(,(concat "^"
               "Block \\(.+\\): "
               "Line \\([0-9]+\\), "
               "Column \\([0-9]+\\):"
               ".*$")
      lb-datalog--block-regexp-to-filename 2 3 nil 1))
  "Compilation error regexp-alist for `lb-datalog-mode' buffers.")

(defvar lb-compilation-error-regexp-alist5
  (eval-when-compile
    `(,(concat "^"
               "File \\(.+\\) (Block .+) : "
               "Lines \\([0-9]+\\)\\(?:-\\([0-9]+\\)\\), "
               "Columns \\([0-9]+\\)-\\([0-9]+\\):"
               "$")
      (1 "%s.logic") (2 . 3) (4 . 5) nil 1))
  "Compilation error regexp-alist for `lb-datalog-mode' buffers.")

(defvar lb-compilation-error-regexp-alist6
  (eval-when-compile
    `(,(concat "^"
               "File \\(.+\\) (Block .+) : "
               "Lines \\([0-9]+\\)\\(?:-\\([0-9]+\\)\\), "
               "Column \\([0-9]+\\):"
               ".*$")
      (1 "%s.logic") (2 . 3) 4 nil 1))
  "Compilation error regexp-alist for `lb-datalog-mode' buffers.")

(defvar lb-compilation-error-regexp-alist7
  (eval-when-compile
    `(,(concat "^"
               "Block \\(.+\\): "
               "Lines \\([0-9]+\\)\\(?:-\\([0-9]+\\)\\), "
               "Columns \\([0-9]+\\)-\\([0-9]+\\):"
               ".*$")
      lb-datalog--block-regexp-to-filename (2 . 3) (4 . 5) nil 1))
  "Compilation error regexp-alist for `lb-datalog-mode' buffers.")

(defvar lb-compilation-error-regexp-alist8
  (eval-when-compile
    `(,(concat "^"
               "Block \\(.+\\): "
               "Lines \\([0-9]+\\)\\(?:-\\([0-9]+\\)\\), "
               "Column \\([0-9]+\\):"
               ".*$")
      lb-datalog--block-regexp-to-filename (2 . 3) 4 nil 1))
  "Compilation error regexp-alist for `lb-datalog-mode' buffers.")


;;----------------------------
;; Add compilation hook
;;----------------------------

(add-hook
 'compilation-mode-hook
 (lambda ()
   (dolist (error-regexp (list lb-compilation-error-regexp-alist1
                               lb-compilation-error-regexp-alist2
                               lb-compilation-error-regexp-alist3
                               lb-compilation-error-regexp-alist4
                               lb-compilation-error-regexp-alist5
                               lb-compilation-error-regexp-alist6
                               lb-compilation-error-regexp-alist7
                               lb-compilation-error-regexp-alist8))
     (add-to-list 'compilation-error-regexp-alist error-regexp))))

(provide 'lb-datalog-compile)

;;; lb-datalog-compile.el ends here
