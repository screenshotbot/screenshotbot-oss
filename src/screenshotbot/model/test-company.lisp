(pkg:define-package :screenshotbot/model/test-company
    (:use #:cl
          #:fiveam
          #:./company)
  (:import-from #:bknr.datastore
                #:delete-object)
  (:import-from #:screenshotbot/user-api
                #:company-name)
  (:import-from #:util/store
                #:with-test-store))
(in-package :screenshotbot/model/test-company)

(util/fiveam:def-suite)

(def-fixture state ()
  (with-test-store ()
    (&body)))

(test jira-config
  (with-fixture state ()
   (let ((company (make-instance 'company)))
     (is-true (jira-config company))
     (is (eql (jira-config company)
              (jira-config company)))
     (delete-object company))))

(test company-name
  (with-fixture state ()
   (let ((company (make-instance 'company
                                  :personalp t
                                  )))
     (unwind-protect
          (progn
            (is (equal "Default" (company-name
                                  company))))
       (delete-object company)))))
