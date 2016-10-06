;; LB Datalog specific step definitions.  All files in this directory
;; whose names end with "-steps.el" will be loaded automatically by
;; Ecukes.

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
