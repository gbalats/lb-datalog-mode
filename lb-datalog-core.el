;;; lb-datalog-core.el -- Core functionality

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

;; This file provides the core functionality of this package, such as
;; movement commands for various elements.

;; Please see lb-datalog-mode.el for more commentary.

;;; Code:


(defgroup lb-datalog nil
  "Major mode `lb-datalog-mode' for editing LB Datalog code."
  :prefix "lb-datalog-"
  :group  'languages)

;;----------------------------
;; Bypass comments
;;----------------------------

(defun lb-datalog-forward-comment (&optional direction)
  "LB Datalog mode specific version of `forward-comment'.
Optional argument DIRECTION defines the direction to move to."
  (let ((comment-start "//")
        (factor (if (< (or direction 0) 0) -99999 99999)))
    (forward-comment factor)))


;;----------------------------
;; Movement by clauses
;;----------------------------

(defun lb-datalog-backward-clause (&optional arg)
  "Move backward to previous clause.
With ARG, repeat.  See `lb-datalog-forward-clause'."
  (interactive "^p")
  (or arg (setq arg 1))
  (lb-datalog-forward-clause (- arg)))

(defun lb-datalog-forward-clause (&optional arg)
  "Move forward to the next clause.
With ARG, repeat.  With negative argument, move ARG times
backward to previous clause."
  (interactive "^p")
  (or arg (setq arg 1))
  ;; moving forward
  (while (> arg 0)
    (lb-datalog-forward-comment 1)      ; bypass comment
    (while                              ; search for dot
        (progn
          (re-search-forward "\\." nil t 1)
          (nth 8 (syntax-ppss))))       ; while ignoring those inside
                                        ; comments or strings
    (setq arg (1- arg)))
  ;; moving backwards
  (while (< arg 0)
    (lb-datalog-forward-comment -1)     ; bypass backward comment
    (backward-char)
    (while                              ; search for previous dot
        (progn
          (re-search-backward "\\`\\|\\." nil t 1)
          (nth 8 (syntax-ppss))))       ; while ignoring those inside
                                        ; comments or strings
    (if (= (char-after) ?\.)
        (forward-char))
    (skip-chars-forward "[:space:]")    ; skip spaces
    (lb-datalog-forward-comment 1)      ; skip comments
    (setq arg (1+ arg))))


(provide 'lb-datalog-core)

;;; lb-datalog-core.el ends here
