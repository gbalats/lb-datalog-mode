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


(defvar lb-datalog-workspace
  nil
  "*The path to an LB Datalog workspace.")

;;;###autoload
(defun lb-datalog-connect (path)
  "Connect to an existing LB Datalog workspace.
The PATH is then stored to the `lb-datalog-workspace' variable."
  (interactive "DConnect to workspace: ")
  (setq lb-datalog-workspace path))


(provide 'lb-datalog-connect)

;;; lb-datalog-connect.el ends here
