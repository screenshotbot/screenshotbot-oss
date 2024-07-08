;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/dashboard/test-image
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/dashboard/image
                #:with-access-checked-image
                #:send-404
                #:%build-resized-image
                #:handle-resized-image)
  (:import-from #:lparallel
                #:force
                #:chain
                #:future)
  (:import-from #:screenshotbot/model/image
                #:make-image
                #:image)
  (:import-from #:bknr.datastore
                #:delete-object)
  (:import-from #:bknr.datastore
                #:blob-pathname)
  (:import-from #:util/store
                #:with-test-store)
  (:import-from #:util/hash-lock
                #:hash-lock)
  (:import-from #:lparallel.kernel
                #:*debug-tasks-p*)
  (:import-from #:screenshotbot/installation
                #:installation
                #:*installation*)
  (:import-from #:util/testing
                #:with-fake-request
                #:with-global-binding)
  (:import-from #:util/threading
                #:*log-sentry-p*)
  (:import-from #:screenshotbot/async
                #:magick-future)
  (:import-from #:util/store/object-id
                #:oid-array
                #:oid)
  (:local-nicknames (#:a #:alexandria)))
(in-package :screenshotbot/dashboard/test-image)


(util/fiveam:def-suite)

(def-fixture state ()
  (with-global-binding ((*installation* (make-instance 'installation))
                        (*log-sentry-p* nil))
   (with-test-store (:globally t)
     (let ((debug-tasks-p *debug-tasks-p*))
       (let* ((im1 #.(asdf:system-relative-pathname
                      :screenshotbot
                      "dashboard/fixture/image.png"))
              (im (make-image :pathname im1 :for-tests t)))
         (unwind-protect
              (progn
                (setf *debug-tasks-p* nil)
                (&body))
           (setf *debug-tasks-p* debug-tasks-p)))))))

(test future-has-*store*
  (with-fixture state ()
    (let ((random-value (random 1000)))
      (symbol-macrolet ((query (list
                                random-value
                                bknr.datastore:*store*
                                util/store:*object-store*)))
        (is (equal query query))
        (is (equal query
                   (force
                    (magick-future ()
                      query))))
        (is (equal query
                   (force (magick-future ()
                            (chain (magick-future () query))))))
        (let ((one (magick-future () query))
              (two (magick-future () query))
              (three (magick-future () query)))
          (is (equal query (force one)))
          (is (equal query (force two)))
          (is (equal query (force three))))))))

(test handle-resized-image-warmup-happy-path
  (with-fixture state ()
    (is (uiop:file-exists-p im1))
    (let ((output-file (handle-resized-image im :tiny :warmup t)))
      (unwind-protect
           (progn
             (is (equal "webp" (pathname-type output-file)))
             (is (equal output-file
                        (handle-resized-image im :tiny :warmup t))))
        (uiop:file-exists-p output-file)))))

(test build-resized-image-for-png
  (with-fixture state ()
    (let ((output-file (%build-resized-image im :tiny :type :png)))
      (is (equal "png" (pathname-type output-file)))
      (%build-resized-image im :tiny :type :png))))

(test build-resized-image-for-png-when-webp-alread-exists
  (with-fixture state ()
    (%build-resized-image im :tiny :type :webp)
    (let ((output-file (%build-resized-image im :tiny :type :png)))
      (is (equal "png" (pathname-type output-file)))
      (%build-resized-image im :tiny :type :png))))


(test send-404-happy-path
  (with-fixture state ()
    (with-fake-request ()
      (is
       (equal "this is a test"
        (catch 'hunchentoot::handler-done
          (send-404 "this is a test")))))))

(test with-access-checked-image
  (with-fixture state ()
    (let ((result-im))
      (with-access-checked-image (image (encrypt:encrypt-mongoid (oid-array im)))
        (setf result-im image))
      (is (eql im result-im)))))
