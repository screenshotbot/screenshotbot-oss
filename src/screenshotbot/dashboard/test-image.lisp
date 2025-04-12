;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/dashboard/test-image
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/dashboard/image
                #:timestamp-is-too-old
                #:*signature-expiry*
                #:quantized-timestamp
                #:%sign-oid
                #:%decode-oid
                #:with-cropped-and-resized
                #:with-access-checked-image
                #:send-404
                #:%build-resized-image
                #:handle-resized-image)
  (:import-from #:lparallel
                #:force
                #:chain
                #:future)
  (:import-from #:screenshotbot/model/image
                #:image-blob-get
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
                #:make-oid
                #:%make-oid
                #:oid-array
                #:oid)
  (:import-from #:screenshotbot/magick/magick-lw
                #:with-wand
                #:magick-get-image-width
                #:magick-get-image-height)
  (:import-from #:fiveam-matchers/core
                #:assert-that
                #:error-with-string-matching
                #:signals-error-matching)
  (:import-from #:fiveam-matchers/strings
                #:matches-regex
                #:contains-string)
  (:import-from #:screenshotbot/screenshot-api
                #:image-public-url)
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

(test |/image/resized.webp happy path|
  (with-fixture state ()
    (with-cropped-and-resized (im 3 3 10 15 2 :output p)
      (with-wand (wand :file p)
        (is (eql 20 (magick-get-image-width wand)))
        (is (eql 30 (magick-get-image-height wand) ))))))

(length (mongoid:oid))

(test %decode-oid-on-eoid
  "This could probably be removed in the future"
  (with-fixture state ()
   (let ((oid (mongoid:oid)))
     (is (equalp oid (%decode-oid (encrypt:encrypt-mongoid oid))))
     (signals-error-matching (timestamp-is-too-old)
       (%decode-oid
        (mongoid:oid-str oid)
        :ts "22"
        :signature "bar"))
     (signals-error-matching ()
       (%decode-oid
        (mongoid:oid-str oid)
        :ts (format nil "~a" (get-universal-time))
        :signature "bar")
       (error-with-string-matching
        (contains-string "signature does not match"))))))

(test %decode-oid-correctly-when-signature-is-present
  "This could probably be removed in the future"
  (with-fixture state ()
    (let* ((oid (mongoid:oid))
           (oid-str (mongoid:oid-str oid)))
      (let ((ts (- (get-universal-time) 500)))
        (is (equalp oid (%decode-oid
                         oid-str
                         :ts (format nil "~a" ts)
                         :signature (%sign-oid oid-str :ts ts))))))))

(test image-public-url
  (is (equal "/image/blob/bar/default.webp" (util:make-url 'image-blob-get :oid "bar"))))

(test image-public-url-originalp
  (with-fixture state ()
    (assert-that (image-public-url im)
                 (matches-regex "/image/blob/.*/default.webp"))
    (assert-that (image-public-url im :originalp t)
                 (matches-regex "/image/original/.*\\.png"))))

(test quantized-timestamp
  (let ((*signature-expiry* 100))
    (is (eql 400 (quantized-timestamp 420)))
    (is (eql 400 (quantized-timestamp 400)))
    (is (eql 450 (quantized-timestamp 470)))))
