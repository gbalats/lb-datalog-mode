;;; lb-datalog-mode-expansions.el --- LB Datalog specific expansions for expand-region

;; Copyright (C) 2014, George Balatsouras
;;
;; Author: George Balatsouras <gbalats(at)gmail(dot)com>
;; Maintainer: George Balatsouras <gbalats(at)gmail(dot)com>
;; Created: 26 Aug 2014
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

;; This package provides some LB Datalog specific expansions that can
;; be used with the `expand-region' package.
;;
;; To use, save `lb-datalog-mode-expansions.el' to a directory in your
;; load-path and add the following to your `.emacs'.
;;
;; (require 'expand-region)
;; (require 'lb-datalog-mode)
;; (require 'lb-datalog-mode-expansions)


;;; Code:


(require 'lb-datalog-core)
(require 'expand-region-core nil 'noerror)


(defun lb-datalog-mark-clause ()
  "Mark one clause, surrounding point.

Intended for use with `expand-region' as an element of
`er/try-expand-list'.  If the point is not inside a clause, there
is no effect."
  (interactive)
  (let ((orig-point (point)))
    (forward-char 1)
    (lb-datalog-backward-clause 1)      ; move to clause beginning
    (set-mark (point))
    (lb-datalog-forward-clause 1)       ; move to clause ending
    (exchange-point-and-mark)
    (when (or (< orig-point (point))    ; undo effects if not inside clause
              (> orig-point (mark)))
      (pop-mark)
      (goto-char orig-point))))

(defun lb-datalog-mark-atom ()
  "Mark one atom, surrounding point.

Intended for use with `expand-region' as an element of
`er/try-expand-list'.  If the point is not inside an atom, there
is no effect."
  (interactive)
  (let ((orig-point (point)))
    (forward-char 1)
    (lb-datalog-backward-atom 1)        ; move to atom beginning
    (set-mark (point))
    (lb-datalog-forward-atom 1)         ; move to atom ending
    (exchange-point-and-mark)
    (when (or (< orig-point (point))    ; undo effects if not inside atom
              (> orig-point (mark)))
      (pop-mark)
      (goto-char orig-point))))

(defun lb-datalog-mark-head-or-body ()
  "Mark the head or body of a clause, surrounding point.

Intended for use with `expand-region' as an element of
`er/try-expand-list'.  If the point is not inside a clause, there
is no effect."
  (interactive)
  (let ((orig-point (point))
        (after-dot-or-arrow nil)
        (start-point nil))
    (forward-char)
    (lb-datalog-backward-clause)        ; move to clause beginning
    (setq start-point (point))          ; store clause beginning
    (while (progn                       ; search for dot, or arrow
             (re-search-forward "\\.\\|->\\|<-" nil t 1)
             (nth 8 (syntax-ppss))))
    (setq after-dot-or-arrow (point))   ; store point after delim and then
    (lb-datalog--skip-delim-backward)   ; move before it
    (if (and (>= orig-point start-point)
             (<= orig-point (point)))   ; check if within head region
        (progn
          (lb-datalog-backward-atom)    ; go to beginning of previous atom
          (lb-datalog-forward-atom)
          (push-mark start-point t nil) ; case in head: mark region
          (exchange-point-and-mark))
      ;; Outside bounds of clause head
      (lb-datalog-forward-atom)         ; go to beginning of next atom
      (lb-datalog-backward-atom)
      (setq start-point (point))        ; store beginning of clause body
      (lb-datalog-forward-clause)
      (lb-datalog--skip-delim-backward) ; move right before dot
      (if (or (< orig-point after-dot-or-arrow)
              (> orig-point (point)))   ; check if within body region
          (goto-char orig-point)        ; case outside clause: do nothing
        (push-mark start-point t nil)   ; case in body: mark region
        (exchange-point-and-mark)))))


(defun er/add-lb-datalog-mode-expansions ()
  "Add LB Datalog specific expansions for buffers in `lb-datalog-mode."
  (set (make-local-variable 'er/try-expand-list)
       (append er/try-expand-list
               '(lb-datalog-mark-atom
                 lb-datalog-mark-head-or-body
                 lb-datalog-mark-clause))))

(when (fboundp 'er/enable-mode-expansions)
  (er/enable-mode-expansions 'lb-datalog-mode
                             'er/add-lb-datalog-mode-expansions))

(provide 'lb-datalog-mode-expansions)

;;; lb-datalog-mode-expansions.el ends here
