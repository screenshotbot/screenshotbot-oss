(pkg:define-package :screenshotbot/model/test-company
    (:use #:cl
          #:fiveam
          #:./company))

(def-suite* :screenshotbot/model/test-company)

(test jira-config
  (let ((company (make-instance 'company)))
    (is-true (jira-config company))
    (is (eql (jira-config company)
             (jira-config company)))))
