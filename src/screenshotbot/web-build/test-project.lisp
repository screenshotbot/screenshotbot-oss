;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/web-build/test-project
  (:use #:cl
        #:fiveam)
  (:import-from #:util/store
                #:with-test-store)
  (:import-from #:screenshotbot/web-build/project
                #:render-time
                #:form
                #:scheduled-job-run-now
                #:actually-run-now
                #:web-project-scheduled-job
                #:update-scheduled-job
                #:web-project)
  (:import-from #:bknr.datastore
                #:object-destroyed-p)
  (:import-from #:screenshotbot/model/company
                #:company)
  (:import-from #:screenshotbot/testing
                #:with-installation
                #:screenshot-test)
  (:import-from #:util/testing
                #:with-fake-request)
  (:local-nicknames (#:a #:alexandria)))
(in-package :screenshotbot/web-build/test-project)


(util/fiveam:def-suite)

(def-fixture state ()
  (with-installation ()
   (with-test-store ()
     (cl-mock:with-mocks ()
       (let* ((company (make-instance 'company))
              (project (make-instance 'web-project :name "foobar"
                                                   :company company)))
         (&body))))))

(test update-scheduled-job
  (with-fixture state ()
    (update-scheduled-job project t "*/3 * * *")
    (let ((first-job (web-project-scheduled-job project)))
      (is-true first-job)
      (update-scheduled-job project t "*/4 * * *")
      (let ((second-job (web-project-scheduled-job project)))
        (is-true second-job)
        (is (not (eql first-job second-job)))
        (is (object-destroyed-p first-job))))))

(def-fixture mocked-actually-run-now ()
  (let ((calls nil))
    (cl-mock:if-called 'actually-run-now
                        (lambda (project &rest args)
                          (declare (ignore args))
                          (push project calls)))
    (&body)))

(test scheduled-job-run-now-running-in-correct-job
  (with-fixture state ()
   (with-fixture mocked-actually-run-now ()
     (update-scheduled-job project t "* * * *")
     (let ((scheduled-jobs:*scheduled-job*
             (web-project-scheduled-job project)))
       (scheduled-job-run-now project))
     (is (equal (list project)
                calls)))))

(test if-scheduled-job-is-incorrect-dont-run
  (with-fixture state ()
    (with-fixture mocked-actually-run-now ()
      (update-scheduled-job project t "* * * *")
      (let ((old-job (web-project-scheduled-job project)))
        (update-scheduled-job project t "* * * *")
        (let ((scheduled-jobs:*scheduled-job*
                old-job))
          (scheduled-job-run-now project)))
      (is (equal nil calls)))))

(def-fixture screenshots ()
  (with-fake-request ()
    (auth:with-sessions ()
      (&body))))

(screenshot-test web-build-new-form
  (with-fixture state ()
    (with-fixture screenshots ()
      (form :submit ""))))

(screenshot-test web-form-with-exclusions
  (with-fixture state ()
    (with-fixture screenshots ()
      (form :submit "" :exclusions "https://goog.gl/foo.txt"))))

(test render-time
  (is (equal "2m:01s"
             (render-time 121)))
  (is (equal "2m:11s"
             (render-time 131)))
    (is (equal "1h:02m:11s"
             (render-time (+ 3600 131)))))
