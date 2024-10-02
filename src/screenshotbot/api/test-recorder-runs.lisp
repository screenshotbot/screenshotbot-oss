;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(pkg:define-package :screenshotbot/api/test-recorder-runs
  (:use #:cl
        #:bknr.datastore
        #:screenshotbot/model/user
        #:screenshotbot/model/channel
        #:screenshotbot/model/image
        #:screenshotbot/model/screenshot
        #:screenshotbot/model/company
        #:screenshotbot/model/api-key
        #:screenshotbot/user-api
        #:fiveam)
  (:import-from #:screenshotbot/server
                #:logged-in-p)
  (:import-from #:screenshotbot/api/recorder-run
                #:production-run-without-ci-permission
                #:validation-error
                #:validate-dto
                #:%put-run
                #:run-to-dto
                #:warmup-image-caches
                #:api-run-put
                #:make-screenshot-for-channel
                #:*synchronous-promotion*
                #:*synchronous-promotion*
                #:%recorder-run-post)
  (:import-from #:util
                #:oid)
  (:import-from #:screenshotbot/testing
                #:with-test-user)
  (:import-from #:util/store
                #:with-test-store)
  (:import-from #:util/testing
                #:with-fake-request)
  (:import-from #:screenshotbot/model/screenshot
                #:*screenshot-cache*)
  (:import-from #:screenshotbot/installation
                #:*installation*
                #:installation
                #:multi-org-feature)
  (:import-from #:cl-mock
                #:if-called
                #:answer)
  (:import-from #:util/object-id
                #:%make-oid
                #:make-oid)
  (:import-from #:screenshotbot/user-api
                #:current-company)
  (:import-from #:screenshotbot/model/image
                #:make-image)
  (:import-from #:screenshotbot/model/recorder-run
                #:trunkp
                #:recorder-run-batch
                #:make-recorder-run
                #:recorder-run)
  (:import-from #:fiveam-matchers/core
                #:has-typep
                #:assert-that
                #:equal-to)
  (:import-from #:fiveam-matchers/strings
                #:starts-with)
  (:import-from #:fiveam-matchers/misc
                #:is-null
                #:is-not-null)
  (:import-from #:screenshotbot/model/batch
                #:batch-commit
                #:batch)
  (:import-from #:auth/viewer-context
                #:api-viewer-context)
  (:import-from #:core/api/model/api-key
                #:api-key-permissions)
  (:import-from #:fiveam-matchers/has-length
                #:has-length)
  (:local-nicknames (#:dto #:screenshotbot/api/model)))

(util/fiveam:def-suite)

(defclass my-installation (multi-org-feature
                           installation)
  ())

(defun fix (name)
  (path:catfile
   #.(asdf:system-relative-pathname :screenshotbot
                                    "fixture/")
   name))

(def-fixture state (&key (api-key-roles :both))
  (dolist (api-key-roles
           (ecase api-key-roles
             (:both
              '(gk:enable gk:disable))
             (:disable
              '(gk:disable))
             (:enable
              '(gk:enable))))
    (let ((*installation* (make-instance 'my-installation)))
      (with-test-store ()
        (gk:create :api-key-roles)
        (funcall api-key-roles :api-key-roles)
        (cl-mock:with-mocks ()
          (with-test-user (:company company
                           :user user)
            (let* ((api-key (make-instance 'api-key
                                           :user user
                                           :company company))
                   (*synchronous-promotion* t)
                   (api-key (make-instance 'api-key :user user :company company
                                                    :permissions '(:ci)))
                   (img1 (make-image :company company :pathname (fix "rose.png")))
                   (img2 (make-image :company company :pathname (fix "wizard.png")))
                   (vc (make-instance 'api-viewer-context
                                      :api-key api-key)))
              (with-fake-request ()
                (auth:with-sessions ()
                  (setf (current-user) user)
                  (setf (current-company) company)
                  (setf (auth:viewer-context hunchentoot:*request*)
                        vc)
                  (assert (logged-in-p))
                  (assert (current-user))
                  (&body))))))))))

(defun serial-recorder-run-post (&rest args)
  (multiple-value-bind (val verify)
      (apply '%recorder-run-post args)
    ;;(funcall verify)
    val))


(test preconditions
  (with-fixture state ()
    (multiple-value-bind (val verify-fn)
        (serial-recorder-run-post
         :channel "foobar"
         :screenshot-records
         (list
          (make-instance 'dto:screenshot
                         :name "img1"
                         :image-id (oid img1))))
      (pass))))

(defun test-adds-channel-mask ()
  (with-fixture state ()
    (let ((channel (make-instance 'channel :company company))
          (rects (list
                  (make-instance 'mask-rect :left 0 :top 1 :width 2 :height 3))))
      (set-channel-screenshot-mask
       channel "img1" rects)
      (let ((screenshot (make-screenshot-for-channel channel
                                                     :name "img2")))
        (is (eql nil (screenshot-masks screenshot))))
      (let ((screenshot (make-screenshot-for-channel channel
                                                     :name "img1")))
        (is (equal rects (screenshot-masks screenshot)))))))

(test adds-channel-mask
  (test-adds-channel-mask))

(test adds-channel-mask-2
  ;; ensure the test fixture is cleaning up properly. In the past,
  ;; there was a time when *screenshot-cache* was not being cleaned up
  ;; properly between tests
  (test-adds-channel-mask))

(test recorder-run-put-happy-path
  (with-fixture state ()
    (let ((dto (make-instance 'dto:run
                              :commit-hash "bleh"
                              :channel "blah"
                              :screenshots (list
                                            (make-instance 'dto:screenshot
                                                           :image-id (oid img1)
                                                           :name "bleh")))))
      (if-called 'warmup-image-caches
                 (lambda (run)))
      (answer (hunchentoot:raw-post-data :force-text t)
        (with-output-to-string (out)
          (yason:encode dto out)))
      (answer (current-company)
        company)
      (finishes
        (api-run-put)))))

(test run-to-dto
  (with-fixture state ()
    (finishes
      (run-to-dto (make-recorder-run
                   :branch "abcd"
                   :screenshots (list
                                 (make-screenshot :image img1 :name "foo")))))))

(test run-to-dto-has-uri
  (with-fixture state ()
    (assert-that
     (dto:recorder-run-url
      (run-to-dto (make-recorder-run
                   :branch "abcd"
                   :screenshots (list
                                 (make-screenshot :image img1 :name "foo")))))
     (starts-with "https://example.com/runs/"))))

(test api-key-can-be-nil-for-populate
  "populate calls into %put-run but provides a NIL :api-key"
  (with-fixture state ()
    (assert company)
    (%put-run company
              (make-instance 'dto:run
                             :channel "foo"
                             :commit-hash "deadbeef"
                             :trunkp t
                             :screenshots (list
                                           (make-instance 'dto:screenshot
                                                          :name "foo"
                                                          :image-id (oid img1))))
              :api-key nil)
    (let ((run (car (last (class-instances 'recorder-run)))))
      (is-true (trunkp run)))))

(test run-with-trunkp-as-t
  (with-fixture state ()
    (assert company)
    (%put-run company
              (make-instance 'dto:run
                             :channel "foo"
                             :commit-hash "deadbeef"
                             :trunkp t
                             :screenshots (list
                                           (make-instance 'dto:screenshot
                                                          :name "foo"
                                                          :image-id (oid img1))))
              :api-key api-key)
    (let ((run (car (last (class-instances 'recorder-run)))))
      (is-true (trunkp run)))))

(test run-with-trunkp-as-t-but-no-ci-permission
  (with-fixture state (:api-key-roles :enable)
    (assert company)
    (setf (api-key-permissions api-key) '(:full))
    (signals production-run-without-ci-permission
      (%put-run company
                (make-instance 'dto:run
                               :channel "foo"
                               :commit-hash "deadbeef"
                               :trunkp t
                               :screenshots (list
                                            (make-instance 'dto:screenshot
                                                           :name "foo"
                                                           :image-id (oid img1))))
                :api-key api-key))
    (assert-that (class-instances 'recorder-run)
                 (has-length 0))))

(test run-with-trunkp-as-t-but-no-ci-permission-with-gk-disabled
  (with-fixture state (:api-key-roles :disable)
    (assert company)
    (setf (api-key-permissions api-key) '(:full))
    (finishes
      (%put-run company
                (make-instance 'dto:run
                               :channel "foo"
                               :commit-hash "deadbeef"
                               :trunkp t
                               :screenshots (list
                                            (make-instance 'dto:screenshot
                                                           :name "foo"
                                                           :image-id (oid img1))))
                :api-key api-key))
    (assert-that (class-instances 'recorder-run)
                 (has-length 1))))

(test batch-is-added
  (with-fixture state ()
    (assert company)
    (%put-run company
              (make-instance 'dto:run
                             :channel "foo"
                             :commit-hash "deadbeef"
                             :batch "dummy-batch"
                             :screenshots (list
                                           (make-instance 'dto:screenshot
                                                          :name "foo"
                                                          :image-id (oid img1)))))
    (let ((run (car (last (class-instances 'recorder-run)))))
      (is-true run)
      (assert-that (recorder-run-batch run)
                   (is-not-null)
                   (has-typep 'batch))
      (is (equal "deadbeef" (batch-commit (recorder-run-batch run)))))))

(test batch-uses-does-not-use-empty-override-commit-hash
  (with-fixture state ()
    (assert company)
    (%put-run company
              (make-instance 'dto:run
                             :channel "foo"
                             :commit-hash "deadbeef"
                             :override-commit-hash ""
                             :batch "dummy-batch"
                             :screenshots (list
                                           (make-instance 'dto:screenshot
                                                          :name "foo"
                                                          :image-id (oid img1)))))
    (let ((run (car (last (class-instances 'recorder-run)))))
      (is-true run)
      (assert-that (recorder-run-batch run)
                   (is-not-null)
                   (has-typep 'batch))
      (is (equal "deadbeef" (batch-commit (recorder-run-batch run)))))))

(test batch-uses-does-not-uses-override-commit-hash
  (with-fixture state ()
    (assert company)
    (%put-run company
              (make-instance 'dto:run
                             :channel "foo"
                             :commit-hash "deadbeef"
                             :override-commit-hash "baadf00d"
                             :batch "dummy-batch"
                             :screenshots (list
                                           (make-instance 'dto:screenshot
                                                          :name "foo"
                                                          :image-id (oid img1)))))
    (let ((run (car (last (class-instances 'recorder-run)))))
      (is-true run)
      (assert-that (recorder-run-batch run)
                   (is-not-null)
                   (has-typep 'batch))
      (is (equal "baadf00d" (batch-commit (recorder-run-batch run)))))))

(test batch-is-nil
  (with-fixture state ()
    (assert company)
    (%put-run company
              (make-instance 'dto:run
                             :channel "foo"
                             :batch nil
                             :screenshots (list
                                           (make-instance 'dto:screenshot
                                                          :name "foo"
                                                          :image-id (oid img1))))
              :api-key api-key)
    (let ((run (car (last (class-instances 'recorder-run)))))
      (is-true run)
      (assert-that (recorder-run-batch run)
                   (is-null)))))


(test validate-tag-name-too-long
  (with-fixture state ()
    (finishes
      (validate-dto (make-instance 'dto:run
                                   :tags nil)))
    (finishes
      (validate-dto (make-instance 'dto:run
                                   :tags (list "foo" "bar"))))
    (signals validation-error
      (validate-dto (make-instance
                     'dto:run
                     :tags (list
                            (make-array 500 :element-type 'character
                                            :initial-element #\a)))))))

