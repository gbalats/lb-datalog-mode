;;; lb-datalog-mode-steps.el -- LB Datalog specific step definitions.

;;; Commentary:

;; All files in this directory whose names end with "-steps.el" will
;; be loaded automatically by Ecukes.

;;; Code:

(require 'lb-datalog-mode nil t)

(And "^I rename symbol to \"\\(.*\\)\"$"
  (lambda (arg)
    (let ((inhibit-message t))
      (call-interactively 'mc--mark-symbol-at-point)
      (call-interactively 'lb-datalog-rename-symbol)
      (And "I press \"DEL\"")
      (And "I type \"%s\"" arg)
      (And "I press \"C-g\""))))

(And "^I move \"\\(.+\\)\" clauses backwards$"
  (lambda (arg) (lb-datalog-backward-clause (string-to-number arg))))

(And "^I move \"\\(.+\\)\" clauses forward$"
  (lambda (arg) (lb-datalog-forward-clause (string-to-number arg))))

(And "^I move \"\\(.+\\)\" atoms backwards$"
  (lambda (arg) (lb-datalog-backward-atom (string-to-number arg))))

(And "^I move \"\\(.+\\)\" atoms forward$"
  (lambda (arg) (lb-datalog-forward-atom (string-to-number arg))))

(When "^I mark the surrounding clause$"
  (lambda () (lb-datalog-mark-clause)))


;;; lb-datalog-mode-steps.el ends here
