(require 'f)

(defvar lb-datalog-mode-support-path
  (f-dirname load-file-name))

(defvar lb-datalog-mode-features-path
  (f-parent lb-datalog-mode-support-path))

(defvar lb-datalog-mode-root-path
  (f-parent lb-datalog-mode-features-path))

(add-to-list 'load-path lb-datalog-mode-root-path)

(require 'espuds)
(require 'ert)
(require 'lb-datalog-mode)

(Setup
 ;; Before anything has run
 )

(Before
 ;; Before each scenario is run
 (switch-to-buffer
  (get-buffer-create "*lb-datalog*"))
 (erase-buffer)
 (transient-mark-mode 1)
 (deactivate-mark))

(After
 ;; After each scenario is run
 )

(Teardown
 ;; After when everything has been run
 )
