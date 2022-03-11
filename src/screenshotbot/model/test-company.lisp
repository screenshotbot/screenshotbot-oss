(pkg:define-package :screenshotbot/model/test-company
    (:use #:cl
          #:fiveam
          #:./company)
  (:import-from #:bknr.datastore
                #:delete-object)
  (:import-from #:screenshotbot/user-api
                #:company-name))
(in-package :screenshotbot/model/test-company)

(util/fiveam:def-suite)

(test jira-config
  (let ((company (make-instance 'company)))
    (is-true (jira-config company))
    (is (eql (jira-config company)
             (jira-config company)))
    (delete-object company)))

(test company-name
  (let ((company (make-instance 'company
                                :personalp t
                                )))
    (unwind-protect
         (progn
           (is (equal "Default" (company-name
                                 company))))
      (delete-object company))))
