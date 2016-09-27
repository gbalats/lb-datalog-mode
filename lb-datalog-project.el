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
(eval-when-compile
  (require 'cl))


;;;###autoload
(defun lb-datalog-find-project-file (&optional path)
  "Return the .project file name, given a PATH (default:
`default-directory')."
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


;;----------------------------
;; Project Type Definition
;;----------------------------

(cl-defstruct lb-datalog-project
  "Project type with metadata."
  name                     ; project name
  filename                 ; the path to the project file
  active-logic             ; a list of file entries marked as active
  inactive-logic           ; a list of file entries marked as inactive
  dependencies)            ; other project dependencies


(defsubst lb-datalog-project-logic (project)
  "Return a list of this PROJECT logic files."
  (-concat (lb-datalog-project-active-logic project)
           (lb-datalog-project-inactive-logic project)))


(defun lb-datalog-parse-project (project-file)
  "Read a project from PROJECT-FILE."
  (let ((project        (make-lb-datalog-project))
        (project-name   nil)
        (active-logic   '())
        (inactive-logic '())
        (dependencies   '()))
    ;; Parse project contents
    (--each (--filter (= 2 (length it))
                      (--map (-map 's-trim (s-split-up-to "," it 2))
                             (s-lines (f-read-bytes project-file))))
      (let ((value (car  it))
            (type  (cadr it)))
        (cond
         ((s-equals? type "active")
          (add-to-list 'active-logic value 'append))
         ((s-equals? type "inactive")
          (add-to-list 'inactive-logic value 'append))
         ((s-equals? type "library")
          (add-to-list 'dependencies value 'append))
         ((s-equals? type "projectname")
          (setq project-name value)))))
    ;; Set project fields
    (setf (lb-datalog-project-filename project) project-file
          (lb-datalog-project-name project) project-name
          (lb-datalog-project-active-logic project) active-logic
          (lb-datalog-project-inactive-logic project) inactive-logic
          (lb-datalog-project-dependencies project) dependencies)
    ;; Return project with all the acquired metadata
    project))


(defmacro lb-datalog-with-project (var &rest body)
  "Bind current project to VAR while executing BODY."
  (declare (debug ((sexp form) body))
           (indent 1))
  `(-when-let
       (,var
        (and (eq major-mode
                 (quote lb-datalog-mode))
             (ignore-errors
               (lb-datalog-parse-project
                (lb-datalog-find-project-file)))))
     ,@body))



;;------------------------------------------------
;; Predicate renaming
;;------------------------------------------------


(defun lb-datalog-read-predicate (prompt)
  "Read the name of a predicate and return as a string.
Prompt with PROMPT."
  (let* ((buffer-predicates '())
         (bounds  (bounds-of-thing-at-point 'symbol))
         (pred-thing (if bounds (buffer-substring-no-properties
                                 (car bounds)
                                 (cdr bounds)))))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "\\([_:?$[:alnum:]]+\\)\\s-*\\s(.*?\\s)" nil t)
        (let ((pred (buffer-substring-no-properties
                     (match-beginning 1)
                     (match-end 1))))
          (unless (member pred buffer-predicates)
            (setq buffer-predicates (cons pred buffer-predicates))))))
    (if (member pred-thing buffer-predicates)
        (completing-read prompt buffer-predicates nil nil pred-thing)
      (completing-read prompt buffer-predicates))))


(define-error 'predicate-already-exists
  "Predicate already exists")

(defun lb-datalog-rename-pred-in-buffer
    (predicate newname &optional ok-if-already-exists)
    "Rename PREDICATE to NEWNAME.  Both args must be strings.
If file has names other than FILE, it continues to have those
names.  Signals a `predicate-already-exists' error if a file
NEWNAME already exists unless optional third argument
OK-IF-ALREADY-EXISTS is non-nil.  A number as third arg means
request confirmation if NEWNAME already exists.  Return the
number of occurrences replaced."
    (interactive
     (let* ((pred-old (lb-datalog-read-predicate "Predicate to rename: "))
            (pred-new (lb-datalog-read-predicate
                       (format "Rename predicate %s to: " pred-old))))
       (list pred-old pred-new)))
    (when (called-interactively-p 'any)
      (setq ok-if-already-exists 1))
    ;; Perform new predicate name check
    (let ((confirm-promp "New predicate name already exists. Rename anyway? ")
          (confirm-rename-p (numberp ok-if-already-exists))
          (pred-regexp (concat (regexp-quote newname) "\\s-*\\s(.*?\\s)")))
      (when (or confirm-rename-p (not ok-if-already-exists))
        (save-excursion
          ;; New predicate name already exists
          (when (re-search-forward pred-regexp nil t)
            ;; Ask for confirmation
            (unless (and confirm-rename-p (y-or-n-p confirm-promp))
              (setq ok-if-already-exists nil))
            ;; Signal error
            (unless ok-if-already-exists
              (signal 'predicate-already-exists (list newname)))))))
    ;; Do the predicate name replacement
    (let ((replacements 0))
      (save-excursion
        (goto-char (point-min))
        ;; Search for each occurrence of predicate
        (while (re-search-forward (regexp-quote predicate) nil t)
          ;; Replace when at the end of predicate's name
          (unless (looking-at "[:$[:alnum:]]")
            (replace-match newname)
            (setq replacements (1+ replacements)))))
      ;; Print message
      (message "Renamed predicate %s to %s in %s"
               predicate newname (buffer-name))
      ;; Return number of replacements
      replacements))


;;------------------------------------------------
;; Add new project entry after saving new file
;;------------------------------------------------


(defun lb-datalog-add-to-project-hook ()
  "Add visiting file to project."
  (lb-datalog-with-project proj
    (let* ((new-file      (buffer-file-name))
           (project-file  (lb-datalog-project-filename proj))
           (logic-files   (lb-datalog-project-logic proj))
           (top-directory (f-dirname project-file))
           (new-entry     (f-relative new-file top-directory)))
     (when (and (f-ext? new-file "logic")
                (not (member new-entry logic-files))
                (y-or-n-p
                 (format "Add to %s? " (f-relative project-file))))
       (let* ((choice
               (read-char-choice
                "Mark as (a)ctive or (i)nactive? " '(?a ?i)))
              (new-entry-type
               (if (char-equal ?a choice) "active" "inactive")))
         (message "Adding %s as %s logic" new-entry new-entry-type)
         ;; Add to project file contents
         (with-current-buffer (find-file-noselect project-file)
           (save-excursion
             (goto-char (point-max))
             ;; insert newline if needed
             (unless (char-equal ?\n (char-before))
               (insert-char ?\n))
             ;; insert new entry
             (insert new-entry ", " new-entry-type ?\n)
             (basic-save-buffer)))))
     ;; Clear echo area
     (run-at-time 1 nil 'message nil))))


;; Mark this hook to run after saving
(add-hook 'after-save-hook
          'lb-datalog-add-to-project-hook)


(provide 'lb-datalog-project)

;;; lb-datalog-project.el ends here
