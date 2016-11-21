;;; lb-datalog-snippets.el -- LB Datalog mode snippets

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

;;; Code:


(require 'yasnippet nil 'noerror)

(defvar lb-datalog-snippets-dir
  (expand-file-name
   "snippets"
   (file-name-directory (or load-file-name (buffer-file-name))))
  "Snippets directory for `lb-datalog-mode'.")

;;;###autoload
(defun lb-datalog-snippets-initialize ()
  "Load snippets for `lb-datalog-mode'."
  (when (file-exists-p lb-datalog-snippets-dir)
    (when (fboundp 'yas-snippet-dirs)
      (add-to-list 'yas-snippet-dirs lb-datalog-snippets-dir t))
    (yas-load-directory lb-datalog-snippets-dir t)))

;;;###autoload
(eval-after-load 'yasnippet
  '(lb-datalog-snippets-initialize))

(provide 'lb-datalog-snippets)
;;; lb-datalog-snippets.el ends here
