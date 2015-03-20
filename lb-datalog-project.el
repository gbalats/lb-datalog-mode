;;; lb-datalog-project.el -- LB Datalog project management

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

;; This file provides various features for managing LB Datalog
;; projects.

;; Please see lb-datalog-mode.el for more commentary.

;;; Code:


(require 'dash)
(require 'f)
(require 's)


(defvar-local lb-datalog-project-file nil
  "The absolute path to the project file.

You should only use this variable inside an
`lb-datalog-with-project' macro.")


;;----------------------------
;; Project File and Contents
;;----------------------------


;;;###autoload
(defun lb-datalog-find-project-file (&optional path)
  "Return the .project file, given a PATH."
  (unless path
    (setq path default-directory))
  (let* ((pattern "*.project")
         (top-directory (f--traverse-upwards (f-glob pattern it) path))
         (project-files (and top-directory (f-glob pattern top-directory))))
    (unless project-files
      (error "No project file"))
    (unless (= 1 (length project-files))
      (error "Multiple project files: %s" project-files))
    (car project-files)))


;;;###autoload
(defun lb-datalog-logic-files (&optional path type)
  "Return a list of logic files that a project comprises.

The list of files is generated by parsing the project file.  The
project is identified by a given PATH, that defaults to the
current directory.  A subset of files can be specified, by
supplying a TYPE argument, which should be one of (i) active,
or (ii) inactive.  For instance, to get the subset of inactive
logic files: (lb-datalog-logic-files 'inactive)."
  (interactive)
  (let ((type-regexp
         (if type (regexp-quote (symbol-name type)) ".*"))
        (project-file
         (lb-datalog-find-project-file (or path default-directory))))
    (--keep (and (= 2 (length it))
                 (s-ends-with? ".logic" (car it))
                 (s-match type-regexp (cadr it))
                 (f-join (f-dirname project-file) (car it)))
            ;; Parse project contents
            (--map (-map 's-trim (s-split-up-to "," it 2))
                   (s-lines (f-read-bytes project-file))))))


(defmacro lb-datalog-with-project (&rest body)
  "Execute BODY only when inside a project.
The path to project file is bound to VAR."
  (declare (debug ((sexp form) body))
           (indent 0))
  `(when (eq major-mode (quote lb-datalog-mode))
     (setq lb-datalog-project-file
           (ignore-errors
             (lb-datalog-find-project-file)))
     (when lb-datalog-project-file
       ,@body)))


(defun lb-datalog-add-to-project-hook ()
  "Add visiting file to project."
  (lb-datalog-with-project
   (let* ((project-file   lb-datalog-project-file)
          (saved-file     (buffer-file-name))
          (project-files  (lb-datalog-logic-files))
          (rel-saved-file (f-relative saved-file
                                      (f-dirname project-file))))
     (when (and (f-ext? saved-file "logic")
                (not (member saved-file project-files)))
       (if (y-or-n-p (format "Add to %s? "
                             (f-relative project-file)))
           (let* ((choice
                   (read-char-choice
                    "Mark as (a)ctive or (i)nactive? " '(?a ?i)))
                  (type
                   (if (char-equal ?a choice) "active" "inactive")))
             (message "Adding %s as %s" rel-saved-file type)
             ;; Add to project file contents
             (with-current-buffer (find-file-noselect project-file)
               (save-excursion
                 (goto-char (point-max))
                 ;; insert newline if needed
                 (unless (char-equal ?\n (char-before))
                   (insert-char ?\n))
                 ;; insert new entry
                 (insert rel-saved-file ", " type ?\n)
                 (basic-save-buffer))))
         (message ""))))))


(add-hook 'after-save-hook
          'lb-datalog-add-to-project-hook)


(provide 'lb-datalog-project)

;;; lb-datalog-project.el ends here
