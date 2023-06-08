;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(pkg:define-package :screenshotbot/api/test-recorder-runs
  (:use #:cl
        #:alexandria
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
                #:make-recorder-run
                #:recorder-run)
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

(def-fixture state ()
  (let ((*installation* (make-instance 'my-installation)))
   (with-test-store ()
     (with-fake-request ()
       (cl-mock:with-mocks ()
        (auth:with-sessions ()
          (with-test-user (:company company
                           :user user)
            (let* ((*synchronous-promotion* t)
                   (api-key (make-instance 'api-key :user user :company company))
                   (img1 (make-image :company company :pathname (fix "rose.png")))
                   (img2 (make-image :company company :pathname (fix "wizard.png"))))
              (setf (current-user) user)
              (setf (current-company) company)
              (assert (logged-in-p))
              (assert (current-user))
              (&body)))))))))

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
