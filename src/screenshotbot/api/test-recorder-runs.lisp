;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(pkg:define-package :screenshotbot/api/test-recorder-runs
  (:use #:cl
        #:alexandria
        #:bknr.datastore
        #:../model/user
        #:../model/channel
        #:../model/image
        #:../model/screenshot
        #:../model/company
        #:../model/api-key
        #:../user-api
        #:fiveam)
  (:import-from #:../server
                #:logged-in-p)
  (:import-from #:./recorder-run
                #:make-screenshot-for-channel
                #:*synchronous-promotion*
                #:*synchronous-promotion*
                #:%recorder-run-post)
  (:import-from #:util
                #:oid)
  (:import-from #:../testing
                #:with-test-user)
  (:import-from #:util/store
                #:with-test-store))

(util/fiveam:def-suite)

(def-fixture state ()
  (with-test-store ()
   (util:with-fake-request ()
     (auth:with-sessions ()
       (with-test-user (:company company
                        :user user)
         (let* ((*synchronous-promotion* t)
                (api-key (make-instance 'api-key :user user :company company))
                (img1 (make-instance 'image
                                      :company company
                                      :hash "foo1"))
                (img2 (make-instance 'image
                                      :company company
                                      :hash "foo2")))
           (setf (current-user) user)
           (assert (logged-in-p))
           (assert (current-user))
           (let ((*current-api-key* api-key))
             (&body))))))))

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
         `(((:foo . "bar")
            (:name . "img1")
            (:image-id . ,(oid img1))))))))

(test adds-channel-mask
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
        (is (eql rects (screenshot-masks screenshot)))))))
