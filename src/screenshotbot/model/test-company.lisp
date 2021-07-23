(pkg:define-package :screenshotbot/model/test-company
    (:use #:cl
          #:fiveam
          #:./company))

(util/fiveam:def-suite)

(test jira-config
  (let ((company (make-instance 'company)))
    (is-true (jira-config company))
    (is (eql (jira-config company)
             (jira-config company)))))
