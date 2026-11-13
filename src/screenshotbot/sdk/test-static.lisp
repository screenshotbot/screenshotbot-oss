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
                #:make-html-provider
                #:schedule-snapshot
                #:static-website/command
                #:parse-config-from-file
                #:find-all-index.htmls
                #:upload-blob)
  (:import-from #:fiveam-matchers/core
                #:is-not
                #:assert-that)
  (:import-from #:fiveam-matchers/lists
                #:contains
                #:has-item)
  (:import-from #:util/digests
                #:md5-file)
  (:import-from #:util/testing
                #:with-local-acceptor)
  (:import-from #:screenshotbot/replay/browser-config
                #:browser-type
                #:browser-config-name)
  (:import-from #:screenshotbot/sdk/api-context
                #:api-context)
  (:import-from #:screenshotbot/sdk/sdk
                #:request)
  (:import-from #:cl-mock
                #:if-called)
  (:local-nicknames (#:a #:alexandria)
                    (#:replay #:screenshotbot/replay/core)
                    (#:git #:screenshotbot/sdk/git)
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
     (let ((api-context (make-instance 'api-context
                                       :key ""
                                       :secret ""
                                       :hostname host)))
       (unwind-protect
            (progn
              (&body))
         (cleanup))))))

(hunchentoot:define-easy-handler (fake-blob-upload :uri "/api/blob/upload"
                                                   :acceptor-names '(test-acceptor))
    (hash type api-key api-secret-key)
  (let ((seq (hunchentoot:raw-post-data :force-binary t
                                        :want-stream nil)))
    (setf *len* (length seq))
    (setf *hex* (ironclad:digest-sequence :md5 seq))))

(test blob-upload
  (with-fixture state ()
    (uiop:with-temporary-file (:pathname p :stream out)
      (write-string "zoidberg" out)
      (finish-output out)
      (let ((md5 (md5-file p)))
        (upload-blob api-context p)
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
        (upload-blob api-context p)
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

(test parse-config-from-file
  (uiop:with-temporary-file (:pathname p :stream s)
    (write-string "[{
 \"name\":\"Google Chrome\",
 \"type\":\"chrome\",
 \"dimensions\": \"24x2\"
}
]" s)
    (finish-output s)
    (let ((configs (parse-config-from-file p)))
      (is (eql 1 (length configs)))
      (is (equal "Google Chrome"
                 (browser-config-name (car configs))))
      (is (equal "chrome"
                 (browser-type (car configs)))))))


(test simple-make-command
  (finishes
   (static-website/command)))

(test make-html-provider
  (is (equal #'find-all-index.htmls (make-html-provider nil)))
  (uiop:with-temporary-file (:pathname html-list :stream stream :direction :output)
    (format stream "hello.html~%~%bar.html~%car.html")
    (finish-output stream)
    (let ((fn (make-html-provider (namestring html-list))))
      (assert-that
       (funcall fn "fake-dir")
       (contains
        "hello.html"
        "bar.html"
        "car.html")))))

(def-fixture schedule-snapshot ()
  (cl-mock:with-mocks ()
    (if-called 'git:merge-base
               (lambda (repo branch-hash commit)
                 (declare (ignore repo branch-hash commit))
                 "abcd"))
    (let ((snapshot (make-instance 'replay:snapshot
                                   :root-urls (list "http://localhost:1234/index.html")))
          (api)
          (content))
      (if-called 'request
                 (lambda (api-context %api &key method ((:content %content)))
                   (declare (ignore api-context method))
                   (setf api %api)
                   (setf content %content)
                   nil))
      (&body))))

(test schedule-snapshot-with-json-endpoint
  (with-fixture schedule-snapshot ()
    (schedule-snapshot nil snapshot
                       :channel "foo"
                       :work-branch "main"
                       :json-endpoint-p t)
    (is (equal "/api/replay/schedule-json" api))
    (is (typep content 'replay:snapshot-request))
    (is (equal "main" (replay:work-branch content)))
    (is (eql nil (replay:snapshot-request-sdk-flags content)))
    ;; Encode the way REQUEST does, and decode the way the server does.
    (let ((decoded (screenshotbot/api/model:decode-json
                    (util/json-mop:json-mop-to-string content)
                    'replay:snapshot-request)))
      (is (equal "foo" (replay:channel-name decoded)))
      (is (equal "main" (replay:work-branch decoded)))
      (is (equal (list "http://localhost:1234/index.html")
                 (replay::root-urls (replay:snapshot decoded)))))))

(test schedule-snapshot-with-cl-store-endpoint
  (with-fixture schedule-snapshot ()
    (schedule-snapshot nil snapshot
                       :channel "foo")
    (is (equal "/api/replay/schedule" api))
    (is (pathnamep content))))
