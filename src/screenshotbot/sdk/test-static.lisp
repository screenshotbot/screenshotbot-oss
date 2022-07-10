;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/sdk/test-static
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/server
                #:acceptor)
  (:import-from #:screenshotbot/sdk/static
                #:find-all-index.htmls
                #:upload-blob)
  (:import-from #:fiveam-matchers/core
                #:is-not
                #:assert-that)
  (:import-from #:fiveam-matchers/lists
                #:has-item)
  (:import-from #:util/digests
                #:md5-file)
  (:import-from #:util/testing
                #:with-local-acceptor)
  (:local-nicknames (#:a #:alexandria)
                    (#:flags :screenshotbot/sdk/flags)))
(in-package :screenshotbot/sdk/test-static)

(util/fiveam:def-suite)

(defvar *hex* nil)
(defvar *len* -1)

(defun cleanup ()
  (setf *hex* nil)
  (setf *len* -1))

(def-fixture state ()
  (cleanup)
  (let ((auto-restart:*global-enable-auto-retries-p* nil))
   (with-local-acceptor (host) ('hunchentoot:easy-acceptor
                                 :name 'test-acceptor)
     (let ((flags:*hostname* host))
       (unwind-protect
            (progn
              (&body))
         (cleanup))))))

(hunchentoot:define-easy-handler (fake-blob-upload :uri "/api/blob/upload"
                                                   :acceptor-names '(test-acceptor))
    (hash type api-key api-secret-key)
  (screenshotbot/api/image:with-raw-post-data-as-tmp-file (p)
    (setf *len* (trivial-file-size:file-size-in-octets p))
    (setf *hex* (md5-file p))))

(test blob-upload
  (with-fixture state ()
    (uiop:with-temporary-file (:pathname p :stream out)
      (write "zoidberg" :stream out)
      (finish-output out)
      (let ((md5 (md5-file p)))
        (upload-blob p)
        (is (equalp *hex* md5))))))

(test binary-blob
  (with-fixture state ()
    (uiop:with-temporary-file (:pathname p :stream out
                               :element-type '(unsigned-byte 8))
      (loop for i from 0 to 255 do
        (write-byte i out))
      (finish-output out)
      (is (eql 256 (trivial-file-size:file-size-in-octets p)))
      (let ((md5 (md5-file p)))
        (upload-blob p)
        (is (eql 256 *len*))
        (is  (equalp *hex* md5))))))

(defun touch (file)
  (ensure-directories-exist file)
  (with-open-file (o file :direction :output)
    (declare (ignore o))))

(test list-all-index.html
  (tmpdir:with-tmpdir (dir)
    (touch (path:catfile dir "index.html"))
    (assert-that
     (find-all-index.htmls dir)
     (has-item "/index.html"))
    (touch (path:catfile dir "foo/index.html"))
    (assert-that
     (find-all-index.htmls dir)
     (has-item "/index.html")
     (has-item "/foo/index.html"))
    (touch (path:catfile dir "bleh.png"))
    (assert-that
     (find-all-index.htmls dir)
     (is-not (has-item "bleh.png")))))
