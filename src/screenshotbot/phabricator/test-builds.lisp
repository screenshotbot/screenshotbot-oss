;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/phabricator/test-builds
  (:use #:cl
        #:fiveam)
  (:import-from #:util/store
                #:with-test-store)
  (:import-from #:screenshotbot/phabricator/builds
                #:send-restart
                #:build-phid
                #:target-phid
                #:%actually-update-status
                #:needs-sync-p
                #:update-diff-status
                #:%send-message
                #:call-in-future
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
  (:import-from #:screenshotbot/phabricator/plugin
                #:phab-instance-for-company)
  (:import-from #:util/phabricator/conduit
                #:phab-instance)
  (:local-nicknames (#:a #:alexandria)))
(in-package :screenshotbot/phabricator/test-builds)


(util/fiveam:def-suite)

(def-fixture state ()
  (cl-mock:with-mocks ()
    (cl-mock:if-called 'call-in-future
                       (lambda (fn)
                         (funcall fn)))
    (with-test-store ()
      (let ((company (make-instance 'company)))

        (cl-mock:if-called 'phab-instance-for-company
                           (lambda (company)
                             (make-instance 'phab-instance)))
        (&body)))))

(test first-update
  (with-fixture state ()
    (cl-mock:if-called '%send-message
                       (lambda (&rest args)
                         (error "should not be called")))
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

(defun get-only-build-info ()
  (car (class-instances 'build-info)))

(test first-status-then-callback
  (with-fixture state ()

    (cl-mock:if-called '%actually-update-status
                       (lambda (phab self type &key details)
                         (error "Should not be called")))

    (update-diff-status company 123
                        :pass)

    (is (eql t (needs-sync-p (get-only-build-info))))

    (let ((res))
      (cl-mock:if-called '%actually-update-status
                         (lambda (phab self type &key details)
                           (is (equal "foobar"
                                      (target-phid self)))
                           (is (equal "carbar"
                                      (build-phid self)))
                           (push type res))
                         :at-start t)
      (with-current-company (company)
        (%update-build
         :diff 123
         :revision 321
         :target-phid "foobar"
         :build-phid "carbar"))
      (is (equal '(:pass) res)))))

(test update-status-after-callback
  (with-fixture state ()
    (let ((res nil))
      (cl-mock:if-called '%actually-update-status
                         (lambda (phab self type &key details)
                           (push type res)))
      (cl-mock:if-called 'send-restart
                         (lambda (&rest args)
                           (error "send-restart should not be called")))
      (with-current-company (company)
        (%update-build
         :diff 123
         :revision 321
         :target-phid "foobar"
         :build-phid "carbar"))


      ;; We'll immediately send the message in this case
      (update-diff-status company 123 :fail)

      (is (equal '(:fail) res)))))

(test update-status-twice
  (with-fixture state ()
    (let ((res nil))
      (cl-mock:if-called '%actually-update-status
                         (lambda (phab self type &key details)
                           (push type res)))
      (cl-mock:if-called 'send-restart
                         (lambda (&rest args)
                           (error "send-restart should not be called")))
      (with-current-company (company)
        (%update-build
         :diff 123
         :revision 321
         :target-phid "foobar"
         :build-phid "carbar"))


      ;; We'll immediately send the message in this case
      (update-diff-status company 123 :fail)
      (is (equal '(:fail) res))

      (cl-mock:if-called '%actually-update-status
                         (lambda (phab self type &key details)
                           (error "should not be called"))
                         :at-start t)
      (let ((res))
        (cl-mock:if-called 'send-restart
                           (lambda (&rest args)
                             (push t res))
                           :at-start t)

        (update-diff-status company 123 :pass)
        (is (equal '(t) res))
        (is-true (needs-sync-p (get-only-build-info))))

      (setf res nil)

      (cl-mock:if-called '%actually-update-status
                         (lambda (phab self type &key details)
                           (push type res))
                         :at-start t)
      (cl-mock:if-called 'send-restart
                         (lambda (&rest args)
                           (error "send-restart should not be called"))
                         :at-start t)
      (with-current-company (company)
        (%update-build
         :diff 123
         :revision 321
         :target-phid "foobar2"
         :build-phid "carbar2"))
      (is (equal '(:pass) res))
      (is-false (needs-sync-p (get-only-build-info)))
      (is (equal "foobar2" (target-phid (get-only-build-info))))
      (is (equal "carbar2" (build-phid (get-only-build-info)))))))
