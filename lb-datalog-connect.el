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
  (declare (indent defun) (debug t))
  `(if (executable-find lb-datalog-cli)
       ,@body
     (user-error "Executable `%s' is missing" lb-datalog-cli)))


;;----------------------------
;; Workspace Type Definition
;;----------------------------

(cl-defstruct lb-datalog-workspace
  "Workspace type with metadata."
  path                     ; project path
  predicates)              ; a list of its predicates


(defvar lb-datalog-current-ws
  nil
  "*The current LB Datalog workspace.")

(defun lb-datalog-current-workspace ()
  "Return the path to the current workspace."
  (when lb-datalog-current-ws
      (lb-datalog-workspace-path lb-datalog-current-ws)))

(defun lb-datalog-ws-predicates (&optional path)
  "Return a list of all predicates of workspace at PATH."
  (unless path
    (setq path (lb-datalog-current-workspace)))
  (cdr (lb-datalog-process-lines "-db" (f-full path) "-list")))


;;;###autoload
(defun lb-datalog-connect (path)
  "Connect to an existing LB Datalog workspace.
The PATH is then stored to the `lb-datalog-workspace' variable."
  (interactive "DConnect to workspace: ")
  (let ((ws    (make-lb-datalog-workspace :path (f-full path)))
        (preds (lb-datalog-ws-predicates path)))
    (setf (lb-datalog-workspace-predicates ws) preds)
    (setq lb-datalog-current-ws ws)))



;;--------------------------
;; Command line interface
;;--------------------------

(defun lb-datalog-process-lines (&rest command-args)
  "Run command with given COMMAND-ARGS.
Return its output as a list of lines."
  (lb-datalog-with-env
   (let ((command (format "%s %s" lb-datalog-cli (s-join " " command-args)))
         (predicates '()))
      (message "Running: %s" command)
      ;; Print query to process buffer
      (with-temp-buffer
        (apply 'call-process
               (append (list lb-datalog-cli nil '(t nil) nil)
                       command-args))
        (goto-char (point-min))
        ;; iterate over buffer lines
        (while (not (= (point) (point-max)))
          (let ((line
                 (buffer-substring-no-properties (point-at-bol)
                                                 (point-at-eol))))
            (setq predicates (cons (s-trim line) predicates))
            (forward-line 1)))
        ;; return list of lines in correct order
        (nreverse predicates)))))

(defun lb-datalog-sentinel (process event)
  "Monitor LB Datalog PROCESS for changing its state via EVENT."
  (cond
   ;; Received process finished event
   ((s-match "finished" event)
    (progn
      (with-current-buffer (process-buffer process)
        (insert "\n")
        (insert (format "Process %s has finished" process)))))
   ;; Default case
   (t (princ
       (format "Process: %s had the event `%s'" process event)))))

(defun lb-datalog-run-command (&rest command-args)
  "Run command with given COMMAND-ARGS.
Return a buffer that contains the output."
  (lb-datalog-with-env
    (let ((output-buffer (generate-new-buffer "*LB Datalog*"))
          (command (format "%s %s" lb-datalog-cli (s-join " " command-args))))
      (message "Running: %s" command)
      ;; Print query to process buffer
      (with-current-buffer output-buffer
        (insert (propertize "Running LB command:"
                            'face 'underline))
        (insert "\n\n")
        (insert (propertize (format "%s\n\n" command)
                            'face '(:foreground "cyan")))
        (insert (propertize "Output:" 'face 'underline))
        (insert "\n\n"))
      ;; Spawn sub-process while setting sentinel
      (set-process-sentinel
       (apply 'start-process
              (append (list "LB Datalog" output-buffer lb-datalog-cli)
                      command-args))
       'lb-datalog-sentinel)
      (display-buffer output-buffer)
      output-buffer)))

(defun lb-datalog-create-workspace (&optional path)
  "Create a new workspace at PATH."
  (unless path
    (setq path (lb-datalog-current-workspace)))
  (lb-datalog-run-command "-db" (f-full path) "-create" "-overwrite"))

(defun lb-datalog-add-to-workspace (code &optional path)
  "Add CODE to workspace residing at PATH."
  (unless path
    (setq path (lb-datalog-current-workspace)))
  (lb-datalog-run-command "-db" (f-full path) "-addBlock" code))

(defun lb-datalog-query-workspace (query-code &optional path)
  "Run QUERY-CODE for the workspace residing at PATH."
  (unless path
    (setq path (lb-datalog-current-workspace)))
  (lb-datalog-run-command "-db" (f-full path) "-query" query-code))

(defun lb-datalog-pred-info-workspace (predicate &optional path)
  "Print information for PREDICATE of workspace residing at PATH."
  (unless path
    (setq path (lb-datalog-current-workspace)))
  (lb-datalog-run-command "-db" (f-full path) "-predInfo" predicate))

(defun lb-datalog-pop-count-workspace (&optional predicates path)
  "List the sizes of PREDICATES for the workspace residing at PATH.

The argument PREDICATES must be a list of predicate names.  If no
PREDICATES are given, list the sizes for all predicates of the
workspace."
  (unless path
    (setq path (lb-datalog-current-workspace)))
  (if predicates
      (lb-datalog-run-command
       "-db" (f-full path) "-popCount" (s-join "," predicates))
    (lb-datalog-run-command "-db" (f-full path) "-popCount")))


;;------------------------
;; Interactive commands
;;------------------------


(defmacro lb-datalog-with-region-and-ws (start end &rest body)
  "Select a region from START to END and execute BODY.
Also connect to workspace before executing BODY, if needed."
  (declare (indent 2) (debug t))
  `(progn
     (unless (or ,start ,end)
       ;; Try selecting enclosing clause as region
       (let ((orig-point (point)))
         (save-excursion
           (forward-char 1)
           (lb-datalog-backward-clause 1)
           (unless (> (point) orig-point)
             (setq ,start (point)))
           (lb-datalog-forward-clause 1)
           (unless (< (point) orig-point)
             (setq ,end (point))))))
     (if (not (and ,start ,end))        ; region must be selected
         (user-error "No active region or clause at point")
       (unless lb-datalog-current-ws    ; connect to workspace
         (call-interactively 'lb-datalog-connect))
       ,@body)))


;;;###autoload
(defun lb-datalog-query (&optional from to)
  "Run some query logic to active workspace.
When called interactively, add current clause or text selection.

When called in LISP code, add the code in the region between
position FROM and TO."
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list nil nil)))
  (lb-datalog-with-region-and-ws from to
    (let ((code-string (buffer-substring-no-properties from to)))
      (lb-datalog-query-workspace code-string))))


;;;###autoload
(defun lb-datalog-add-block (&optional from to)
  "Add some logic to active workspace.
When called interactively, add current clause or text selection.

When called in LISP code, add the code in the region between
position FROM and TO."
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list nil nil)))
  (lb-datalog-with-region-and-ws from to
    (let ((code-string (buffer-substring-no-properties from to)))
      (lb-datalog-add-to-workspace code-string))))

;;;###autoload
(defun lb-datalog-pop-count ()
  "List all predicate sizes of active workspace."
  (interactive)
  (unless lb-datalog-current-ws         ; connect to workspace
    (call-interactively 'lb-datalog-connect))
  (lb-datalog-pop-count-workspace))

;;;###autoload
(defun lb-datalog-pred-info (predicate)
  "Print information for PREDICATE."
  (interactive
   (list
    (progn
      (unless lb-datalog-current-ws         ; connect to workspace
        (call-interactively 'lb-datalog-connect))
      (let ((ws lb-datalog-current-ws))
        (completing-read "Enter predicate: "
                         (lb-datalog-workspace-predicates ws))))))
  (lb-datalog-pred-info-workspace predicate))

(provide 'lb-datalog-connect)

;;; lb-datalog-connect.el ends here
