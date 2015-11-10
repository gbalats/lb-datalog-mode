;;; lb-datalog-connect.el -- LB Datalog workspace management

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

;; This file provides various features for querying LB Datalog
;; workspaces.

;; Please see lb-datalog-mode.el for more commentary.

;;; Code:

(require 'f)
(require 's)
(require 'lb-datalog-core)

(defcustom lb-datalog-cli "bloxbatch"
    "Return the name of the LB command line utility.
If the command is not on your path, you may specify a fully
qualified path to it."
    :tag "LB command line utility"
    :group 'lb-datalog
    :type 'string)

(defmacro lb-datalog-with-env (&rest body)
  "Ensure command environment is set before executing BODY."
  (declare (indent 0) (debug t))
  `(if (executable-find lb-datalog-cli)
       ,@body
     (user-error "Executable `%s' is missing" lb-datalog-cli)))

(defvar lb-datalog-workspace
  nil
  "*The path to an LB Datalog workspace.")

;;;###autoload
(defun lb-datalog-connect (path)
  "Connect to an existing LB Datalog workspace.
The PATH is then stored to the `lb-datalog-workspace' variable."
  (interactive "DConnect to workspace: ")
  (setq lb-datalog-workspace path))


;;--------------------------
;; Command line interface
;;--------------------------

(defun lb-datalog-run-command (&rest command-args)
  "Run command with given COMMAND-ARGS.
Return a buffer that contains the output."
  (lb-datalog-with-env
    (let ((output-buffer (generate-new-buffer "*LB Datalog*"))
          (command (format "%s %s" lb-datalog-cli (s-join " " command-args))))
      (message "Running: %s" command)
      (with-current-buffer output-buffer
        (insert (propertize (format "%s\n\n" command)
                            'face 'bold)))
      (apply 'start-process
             (append (list "LB Datalog" output-buffer lb-datalog-cli)
                     command-args))
      (display-buffer output-buffer)
      output-buffer)))

(defun lb-datalog-create-workspace (&optional path)
  "Create a new workspace at PATH."
  (unless path
    (setq path lb-datalog-workspace))
  (lb-datalog-run-command "-db" (f-full path) "-create" "-overwrite"))

(defun lb-datalog-add-to-workspace (code &optional path)
  "Add CODE to workspace residing at PATH."
  (unless path
    (setq path lb-datalog-workspace))
  (lb-datalog-run-command "-db" (f-full path) "-addBlock" code))

(defun lb-datalog-query-workspace (query-code &optional path)
  "Run QUERY-CODE for the workspace residing at PATH."
  (unless path
    (setq path lb-datalog-workspace))
  (lb-datalog-run-command "-db" (f-full path) "-query" query-code))


;;------------------------
;; Interactive commands
;;------------------------

;;;###autoload
(defun lb-datalog-add-block (&optional from to)
  "Add some logic to active workspace.
When called interactively, add current clause or text selection.

When called in LISP code, add the code in the region between
position FROM and TO."
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (save-excursion
       (forward-char 1)
       (lb-datalog-backward-clause 1)
       (setq from (point))
       (lb-datalog-forward-clause 1)
       (setq to (point))
       (list from to))))
  ;; Connect to workspace
  (unless lb-datalog-workspace
    (call-interactively 'lb-datalog-connect))
  ;; Add block of code
  (let ((code-string (buffer-substring-no-properties from to)))
    (lb-datalog-add-to-workspace code-string)))

(provide 'lb-datalog-connect)

;;; lb-datalog-connect.el ends here
