;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/replay/test-replay-acceptor
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/replay/replay-acceptor
                #:push-snapshot
                #:render-acceptor
                #:default-render-acceptor
                #:*default-render-acceptor*)
  (:import-from #:util/testing
                #:with-local-acceptor)
  (:import-from #:screenshotbot/replay/core
                #:http-header
                #:assets
                #:uuid
                #:snapshot
                #:asset)
  (:import-from #:util/store
                #:with-test-store)
  (:import-from #:screenshotbot/model/company
                #:company)
  (:local-nicknames (#:a #:alexandria)))
(in-package :screenshotbot/replay/test-replay-acceptor)

(util/fiveam:def-suite)

(defparameter *fixture* (asdf:system-relative-pathname
                         :screenshotbot
                         "fixture/rose.png"))

(def-fixture state ()
  (with-test-store ()
    (tmpdir:with-tmpdir (tmpdir)
     (with-local-acceptor (host :acceptor acceptor)
         ('render-acceptor)
       (fad:copy-file *fixture* (path:catfile tmpdir "abcd00.png"))
       (let* ((*default-render-acceptor* acceptor)
              (company (make-instance 'company))
              (snapshot (make-instance 'snapshot
                                        :tmpdir tmpdir))
              (asset (make-instance
                      'asset
                       :status 200
                       :url "https://www.example.com"
                       :file (format nil "/snapshot/~a/assets/abcd00.png"
                                     (uuid snapshot))
                       :response-headers
                       (list (make-instance 'http-header
                                             :name "Content-type"
                                             :value "image/png")))))
         (setf (assets snapshot)
               (list asset))
         (&body))))))

(test simple-loading
  (with-fixture state ()
    (push-snapshot acceptor company snapshot)
    (let ((url (format nil "~a/snapshot/~a/assets/abcd00.png"
                       host
                       (uuid snapshot))))
      (multiple-value-bind (stream ret)
          (dex:get url
                   :force-binary t
                   :want-stream t)
        (with-open-stream (stream stream)
          (is (equal 200 ret))
          (is (equalp (md5:md5sum-file *fixture*)
                      (md5:md5sum-stream stream))))))))


(test expect-404-for-non-existent-assets
  (with-fixture state ()
    (push-snapshot acceptor company snapshot)
    (let ((url (format nil "~a/snapshot/~a/assets/abcd01.png"
                       host
                       (uuid snapshot))))
      (signals dex:http-request-not-found
        (dex:get url)))))
