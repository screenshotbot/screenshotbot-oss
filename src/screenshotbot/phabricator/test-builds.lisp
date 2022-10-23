(defpackage :screenshotbot/phabricator/test-builds
  (:use #:cl
        #:fiveam)
  (:import-from #:util/store
                #:with-test-store)
  (:import-from #:screenshotbot/phabricator/builds
                #:find-build-info
                #:build-info
                #:%update-build)
  (:import-from #:util/testing
                #:with-fake-request)
  (:import-from #:bknr.datastore
                #:class-instances)
  (:import-from #:screenshotbot/model/company
                #:company)
  (:import-from #:screenshotbot/login/common
                #:with-current-company)
  (:local-nicknames (#:a #:alexandria)))
(in-package :screenshotbot/phabricator/test-builds)


(util/fiveam:def-suite)

(def-fixture state ()
  (with-test-store ()
    (let ((company (make-instance 'company)))
     (&body))))

(test first-update
  (with-fixture state ()
    (with-current-company (company)
      (%update-build
       :diff 123
       :revision 321
       :target-phid "foobar"
       :build-phid "carbar"))
    (let ((info (car (class-instances 'build-info))))
      (is (eql company
               (company info)))
      (is (eql info
               (find-build-info company 123)))

      (with-current-company (company)
       (%update-build
        :diff 123
        :revision 321
        :target-phid "new-foo"
        :build-phid "new-car"))
      (is (equal (list info) (class-instances 'build-info))))))
