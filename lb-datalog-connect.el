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

(require 'dash)
(require 'f)
(require 's)
(require 'thingatpt)
(require 'lb-datalog-core)


(defcustom lb-cmd-exe "bloxbatch"
    "Return the name of the LB command line utility.
If the command is not on your path, you may specify a fully
qualified path to it."
    :tag "LB command line utility"
    :group 'lb-datalog
    :type 'string)

(defmacro lb-cmd-check-env (&rest body)
  "Ensure command environment is set before executing BODY."
  (declare (indent defun) (debug t))
  `(if (executable-find lb-cmd-exe)
       ,@body
     (user-error "Executable `%s' is missing" lb-cmd-exe)))

;;----------------------------
;; Workspace Type Definition
;;----------------------------

(cl-defstruct (lb-workspace (:conc-name lb-ws-))
  "Connection to an LB workspace."
  path                     ; workspace path
  predicates)              ; a cached list of the workspace predicates

(defvar lb-open-connections
  nil
  "The currently open LB Datalog workspace connections.")

(defun lb-workspace-path (&optional ws)
  "Return the path to the last connected workspace or WS."
  (if (not (or ws lb-open-connections))
      (user-error "No open workspace connections")
    (lb-ws-path (or ws (car lb-open-connections)))))

(defun lb-workspace-predicates (&optional ws)
  "Return a list of predicates of the last connected workspace or WS."
  (if (not (or ws lb-open-connections))
      (user-error "No open workspace connections")
    (lb-ws-predicates (or ws (car lb-open-connections)))))

;;;###autoload
(defun lb-datalog-connect (path &optional keep)
  "Connect to an existing LB Datalog workspace at PATH.

If KEEP is true, then any previous connections are preserved."
  (interactive "DConnect to workspace: \nP")
  (let ((connection
         (make-lb-workspace
          :path (f-full path)
          :predicates (lb-cmd-list-predicates path))))
    (if keep (push connection lb-open-connections)
      (setq lb-open-connections (list connection)))))

;;;###autoload
(defun lb-datalog-disconnect ()
  "Disconnect from the newest currently open workspace."
  (interactive)
  (cond (lb-open-connections
         (let ((connection (pop lb-open-connections)))
           (message "Disconnected from workspace at %s"
                    (lb-workspace-path connection))))
        (t (user-error "No open workspace connections"))))


;;--------------------------
;; Command line interface
;;--------------------------

(defun lb-cmd-sentinel (process event)
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

(defun lb-cmd--process-lines (&rest command-args)
  "Run command with given COMMAND-ARGS.
Return its output as a list of lines."
  (lb-cmd-check-env
   (let ((command (format "%s %s" lb-cmd-exe (s-join " " command-args)))
         (predicates '()))
     (with-temp-message (format "Running: %s" command)
       (with-temp-buffer
         (apply 'call-process
                (append (list lb-cmd-exe nil '(t nil) nil)
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
         (nreverse predicates))))))

(defun lb-cmd--run-command (&rest command-args)
  "Run command with given COMMAND-ARGS.
Return a buffer that contains the output."
  (lb-cmd-check-env
    (let ((output-buffer (generate-new-buffer "*LB Datalog*"))
          (command (format "%s %s" lb-cmd-exe (s-join " " command-args))))
      (with-temp-message (format "Running: %s" command)
        (with-current-buffer output-buffer
          ;; Print query to process buffer
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
                (append (list "LB Datalog" output-buffer lb-cmd-exe)
                        command-args))
         'lb-cmd-sentinel)
        (display-buffer output-buffer)
        output-buffer))))

(defun lb-cmd-create (&optional path)
  "Create a new workspace at PATH."
  (unless path
    (setq path (lb-workspace-path)))
  (lb-cmd--run-command "-db" (f-full path) "-create" "-overwrite"))

(defun lb-cmd-add (code &optional path)
  "Add CODE to workspace residing at PATH."
  (unless path
    (setq path (lb-workspace-path)))
  (lb-cmd--run-command "-db" (f-full path) "-addBlock" code))

(defun lb-cmd-query (query-code &optional path)
  "Run QUERY-CODE for the workspace residing at PATH."
  (unless path
    (setq path (lb-workspace-path)))
  (lb-cmd--run-command "-db" (f-full path) "-query" query-code))

(defun lb-cmd-pred-info (predicate &optional path)
  "Print information for PREDICATE of workspace residing at PATH."
  (unless path
    (setq path (lb-workspace-path)))
  (lb-cmd--run-command "-db" (f-full path) "-predInfo" predicate))

(defun lb-cmd-pred-print (predicate &optional path)
  "Print all rows of PREDICATE of workspace residing at PATH."
  (unless path
    (setq path (lb-workspace-path)))
  (lb-cmd--run-command "-db" (f-full path) "-print" predicate))

(defun lb-cmd-pop-count (&optional predicates path)
  "List the sizes of PREDICATES for the workspace residing at PATH.

The argument PREDICATES must be a list of predicate names.  If no
PREDICATES are given, list the sizes for all predicates of the
workspace."
  (unless path
    (setq path (lb-workspace-path)))
  (cond ((not predicates)
         (lb-cmd--run-command "-db" (f-full path) "-popCount"))
        ((listp predicates)
         (lb-cmd--run-command "-db" (f-full path)
                              "-popCount" (s-join "," predicates)))
        ((stringp predicates)
         (lb-cmd--run-command "-db" (f-full path)
                              "-popCount" (s-join ","
                                                  (split-string predicates))))
        (t (user-error "Unknown type of predicates (neither list or string)"))))

(defun lb-cmd-list-predicates (&optional path)
  "Return a list of all predicates of workspace at PATH."
  (unless path
    (setq path (lb-workspace-path)))
  (cdr (lb-cmd--process-lines "-db" (f-full path) "-list")))


;;------------------------
;; Interactive commands
;;------------------------


(defmacro lb-datalog-with-connection (&rest body)
  "Connect to some workspace before executing BODY.

This has no effect if an open connection already exists.  Also,
the connection to the workspace is bound to WS, while
executing BODY."
  (declare (indent defun) (debug t))
  `(progn
     (unless lb-open-connections ; connect to some workspace first
       (command-execute 'lb-datalog-connect))
     (let* ((ws (car lb-open-connections)))
       ,@body)))

(defun lb-datalog-clause-or-region-text ()
  "Return the text of the clause at point or the selected region."
  (let ((from nil)
        (to   nil))
    (cond ((use-region-p)               ; on selected region
           (setq from (region-beginning)
                 to   (region-end)))
          (t                            ; on surrounding clause
           (let ((bounds (lb-datalog-bounds-of-clause-at-point)))
             (setq from (car bounds)
                   to   (cdr bounds)))))
    (if (not (and from to))             ; region must be selected
        (user-error "No active region or clause at point")
      (buffer-substring-no-properties from to))))

;;;###autoload
(defun lb-datalog-query ()
  "Run query logic inside the selected region on active workspace.

If no region is selected, then use the clause at point."
  (interactive)
  (lb-datalog-with-connection
    (let ((code-string (lb-datalog-clause-or-region-text)))
      (lb-cmd-query code-string))))

;;;###autoload
(defun lb-datalog-query-all ()
  "Run query logic inside the selected region on all open workspaces.

If no region is selected, then use the clause at point."
  (interactive)
  (let ((code-string (lb-datalog-clause-or-region-text)))
    (--each lb-open-connections
      (lb-cmd-query code-string (lb-workspace-path it)))))

;;;###autoload
(defun lb-datalog-add-block ()
  "Add logic of selected region to active workspace.

If no region is selected, then use the clause at point."
  (interactive)
  (lb-datalog-with-connection
    (let ((code-string (lb-datalog-clause-or-region-text)))
      (lb-cmd-add code-string))))

;;;###autoload
(defun lb-datalog-pop-count ()
  "List all predicate sizes of active workspace."
  (interactive)
  (lb-datalog-with-connection
    (lb-cmd-pop-count)))

(defun lb-datalog-prompt-pred ()
  "Prompt for a predicate name defined in the connected workspace."
  (lb-datalog-with-connection
    (let* ((all-preds  (lb-workspace-predicates))
           (bounds     (bounds-of-thing-at-point 'symbol))
           (pred-thing (if bounds (buffer-substring-no-properties
                                   (car bounds)
                                   (cdr bounds)))))
      (if (member pred-thing all-preds)
          (completing-read "Enter predicate: " all-preds nil nil pred-thing)
        (completing-read "Enter predicate: " all-preds)))))

;;;###autoload
(defun lb-datalog-pred-info (predicate)
  "Print information for PREDICATE on active workspace."
  (interactive (list (lb-datalog-prompt-pred)))
  (lb-datalog-with-connection
    (lb-cmd-pred-info predicate (lb-workspace-path))))

;;;###autoload
(defun lb-datalog-pred-print (predicate)
  "Print PREDICATE contents on active workspace."
  (interactive (list (lb-datalog-prompt-pred)))
  (lb-datalog-with-connection
    (lb-cmd-pred-print predicate (lb-workspace-path))))

(provide 'lb-datalog-connect)

;;; lb-datalog-connect.el ends here
