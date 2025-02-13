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
                #:recorder-run-metadata
                #:shard
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
                #:transient-api-key
                #:cli-api-key
                #:api-key-permissions)
  (:import-from #:fiveam-matchers/has-length
                #:has-length)
  (:import-from #:fiveam-matchers/lists
                #:has-item
                #:contains)
  (:import-from #:bknr.datastore
                #:class-instances)
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

(defun call-api-put-run (company dto)
  (if-called 'warmup-image-caches
             (lambda (run) (declare (ignore run))))
  (answer (hunchentoot:raw-post-data :force-text t)
    (with-output-to-string (out)
      (yason:encode dto out)))
  (answer (current-company)
    company)
  (api-run-put))

(test recorder-run-put-happy-path
  (with-fixture state ()
    (let ((dto (make-instance 'dto:run
                              :commit-hash "bleh"
                              :channel "blah"
                              :screenshots (list
                                            (make-instance 'dto:screenshot
                                                           :image-id (oid img1)
                                                           :name "bleh")))))
      (finishes
        (call-api-put-run company dto)))))

(test recorder-run-put-only-shard
  (with-fixture state ()
    (let ((dto (make-instance 'dto:run
                              :commit-hash "bleh"
                              :channel "blah"
                              :shard-spec (make-instance 'dto:shard-spec
                                                         :key "foobar"
                                                         :number 0
                                                         :count 2)
                              :screenshots (list
                                            (make-instance 'dto:screenshot
                                                           :image-id (oid img1)
                                                           :name "bleh")))))
      (finishes
       (call-api-put-run company dto)))))

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

(test run-with-trunkp-as-t-for-cli-api-key
  (with-fixture state (:api-key-roles :enable)
    (assert company)
    (let ((api-key (make-instance 'cli-api-key
                                  :user user
                                  :permissions '(:ci)
                                  :company company)))
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
                  :api-key api-key)))
    (assert-that (class-instances 'recorder-run)
                 (has-length 0))))

(test run-with-trunkp-as-t-for-transient-api-key
  (with-fixture state ()
    (assert company)
    (let ((api-key (make-instance 'transient-api-key
                                  :user user
                                  :company company)))
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

(test metadata-is-saved
  (with-fixture state ()
    (finishes
      (%put-run company
                (make-instance 'dto:run
                               :channel "foo"
                               :commit-hash "deadbeef"
                               :metadata (list
                                          (make-instance 'dto:metadata
                                                         :key "foo"
                                                         :value "bar"))
                               :screenshots (list
                                            (make-instance 'dto:screenshot
                                                           :name "foo"
                                                           :image-id (oid img1))))
                :api-key api-key))
    (assert-that (class-instances 'recorder-run)
                 (has-length 1))
    (let ((run (first (class-instances 'recorder-run))))
      (assert-that (recorder-run-metadata run)
                   (has-item 
                    '("foo" . "bar"))))))


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

(defun make-long-string (&key (length 1000))
  (make-string length :initial-element #\a))

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

(test validate-metadata-too-long
  (with-fixture state ()
    (finishes
      (validate-dto (make-instance 'dto:run
                                   :metadata nil)))
    (finishes
      (validate-dto (make-instance 'dto:run
                                   :metadata (list
                                              (make-instance 'dto:metadata
                                                             :key "foo"
                                                             :value "bar")))))
    (signals validation-error
      (validate-dto (make-instance
                     'dto:run
                     :metadata (list
                                (make-instance 'dto:metadata
                                               :key (make-long-string :length 500)
                                               :value "foo")))))
    (signals validation-error
      (validate-dto (make-instance
                     'dto:run
                     :metadata (list
                                (make-instance 'dto:metadata
                                               :key "foo"
                                               :value (make-long-string :length 10500))))))))

(test too-many-metadata
  (with-fixture state ()
    (signals validation-error
      (validate-dto (make-instance
                     'dto:run
                     :metadata (loop for i from 1 to 100
                                     collect
                                     (make-instance 'dto:metadata
                                                    :key "foo"
                                                    :value "value")))))))

(test if-a-shard-spec-is-present-we-dont-create-a-run-immediately
  (with-fixture state ()
    (assert company)
    (%put-run company
              (make-instance 'dto:run
                             :channel "foo"
                             :batch nil
                             :shard-spec (make-instance 'dto:shard-spec
                                                        :key "shard-key"
                                                        :number 0
                                                        :count 10)
                             :screenshots (list
                                           (make-instance 'dto:screenshot
                                                          :name "foo"
                                                          :image-id (oid img1))))
              :api-key api-key)
    (assert-that (class-instances 'recorder-run)
                 (has-length 0))
    (assert-that (class-instances 'shard)
                 (has-length 1))))


(test for-the-last-shard-create-the-run-immediately
  (with-fixture state ()
    (assert company)
    (loop for i from 1 below 10
          do
             (make-instance 'shard
                            :key "shard-key"
                            :screenshots (list
                                          (make-screenshot
                                           :name (format nil "img~a" i)
                                           :image img1))
                            :channel (find-or-create-channel company "foo")
                            :number i
                            :count 10))
    (assert-that
     (%put-run company
               (make-instance 'dto:run
                              :channel "foo"
                              :batch nil
                              :shard-spec (make-instance 'dto:shard-spec
                                                         :key "shard-key"
                                                         :number 0
                                                         :count 10)
                              :screenshots (list
                                            (make-instance 'dto:screenshot
                                                           :name "foo"
                                                           :image-id (oid img1))))
               :api-key api-key)
     (contains
      (has-typep t)
      (has-typep 'recorder-run)
      (has-typep t)))
    (assert-that (class-instances 'recorder-run)
                 (has-length 1))
    (let ((run (car (class-instances 'recorder-run))))
      (assert-that (recorder-run-screenshots run)
                   (has-length 10)))))

(test we-also-handle-1-indexed-shards
  (with-fixture state ()
    (assert company)
    (loop for i from 1 below 10
          do
             (make-instance 'shard
                            :key "shard-key"
                            :screenshots (list
                                          (make-screenshot
                                           :name (format nil "img~a" i)
                                           :image img1))
                            :channel (find-or-create-channel company "foo")
                            :number i
                            :count 10))
    (assert-that
     (%put-run company
               (make-instance 'dto:run
                              :channel "foo"
                              :batch nil
                              :shard-spec (make-instance 'dto:shard-spec
                                                         :key "shard-key"
                                                         :number 10
                                                         :count 10)
                              :screenshots (list
                                            (make-instance 'dto:screenshot
                                                           :name "foo"
                                                           :image-id (oid img1))))
               :api-key api-key)
     (contains
      (has-typep t)
      (has-typep 'recorder-run)
      (has-typep t)))
    (assert-that (class-instances 'recorder-run)
                 (has-length 1))
    (let ((run (car (class-instances 'recorder-run))))
      (assert-that (recorder-run-screenshots run)
                   (has-length 10)))))

(test we-dont-create-the-run-if-a-shard-was-repeated
  (with-fixture state ()
    (assert company)
    (loop for i from 1 below 10
          do
             (make-instance 'shard
                            :key "shard-key"
                            :screenshots (list
                                          (make-screenshot
                                           :name (format nil "img~a" i)
                                           :image img1))
                            :channel (find-or-create-channel company "foo")
                            :number i
                            :count 10))
    (%put-run company
              (make-instance 'dto:run
                             :channel "foo"
                             :batch nil
                             :shard-spec (make-instance 'dto:shard-spec
                                                        :key "shard-key"
                                                        :number 1
                                                        :count 10)
                             :screenshots (list
                                           (make-instance 'dto:screenshot
                                                          :name "foo"
                                                          :image-id (oid img1))))
              :api-key api-key)
    (assert-that (class-instances 'recorder-run)
                 (has-length 0))))

(test once-the-run-is-created-we-dont-create-more-runs-for-the-shard
  (with-fixture state ()
    (loop for i from 1 below 10
          do
             (make-instance 'shard
                            :key "shard-key"
                            :screenshots (list
                                          (make-screenshot
                                           :name (format nil "img~a" i)
                                           :image img1))
                            :channel (find-or-create-channel company "foo")
                            :number i
                            :count 10))
    (dotimes (i 2)
     (%put-run company
               (make-instance 'dto:run
                              :channel "foo"
                              :batch nil
                              :shard-spec (make-instance 'dto:shard-spec
                                                         :key "shard-key"
                                                         :number 0
                                                         :count 10)
                              :screenshots (list
                                            (make-instance 'dto:screenshot
                                                           :name "foo"
                                                           :image-id (oid img1))))
               :api-key api-key))
    (assert-that (class-instances 'recorder-run)
                 (has-length 1))
    (let ((run (car (class-instances 'recorder-run))))
      (assert-that (recorder-run-screenshots run)
                   (has-length 10)))))

(test validation-failure-for-long-shard-key-name
  (finishes
    (validate-dto
     (make-instance 'dto:run
                    :shard-spec (make-instance 'dto:shard-spec
                                               :key "shard-key"
                                               :number 0
                                               :count 20))))
  (signals validation-error
    (validate-dto
     (make-instance 'dto:run
                    :shard-spec (make-instance 'dto:shard-spec
                                               :key (make-long-string)
                                               :number 0
                                               :count 20)))))



