;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/sdk/test-fetch-run
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/sdk/fetch-run
                #:Safe-name-p
                #:unsafe-screenshot-name
                #:*download-engine*
                #:%save-run)
  (:import-from #:util/request
                #:http-request-impl)
  (:local-nicknames (#:dto #:screenshotbot/api/model)))
(in-package :screenshotbot/sdk/test-fetch-run)

(util/fiveam:def-suite)

(defclass fake-engine ()
  ())

(defmethod http-request-impl ((engine fake-engine) url &key &allow-other-keys)
  (flex:make-in-memory-input-stream
   (flex:string-to-octets "foobar")))

(def-fixture state (&key (screenshot-name "foo"))
  (cl-mock:with-mocks ()
    (let ((*download-engine* (make-instance 'fake-engine)))
     (let ((run (make-instance 'dto:run
                               :screenshots
                               (list
                                (make-instance 'dto:screenshot
                                               :name screenshot-name
                                               :url "https://example.com")))))
       (&body)))))

(test simple-save-run
  (with-fixture state ()
    (tmpdir:with-tmpdir (dir)
      (%save-run run :output dir)
      (is (uiop:file-exists-p (path:catfile  dir "foo.png")))
      (is (equal "foobar" (uiop:read-file-string (path:catfile  dir "foo.png")))))))

(test crashes-on-trying-to-write-to-different-directory
  (with-fixture state (:screenshot-name "/../car/bar")
    (tmpdir:with-tmpdir (dir)
      (signals unsafe-screenshot-name
        (%save-run run :output dir)))))

(test safe-name-p-on-some-options
  (with-fixture state ()
    (is-true (Safe-name-p "foobar"))
    (is-false (safe-name-p "/bar/car"))
    (is-false (Safe-name-p "../car/bar"))))

(test nested-directory
  (with-fixture state (:screenshot-name "foo/bar")
    (tmpdir:with-tmpdir (dir)
      (%save-run run :output dir)
      (is (uiop:file-exists-p (path:catfile  dir "foo/bar.png")))
      (is (equal "foobar" (uiop:read-file-string (path:catfile  dir "foo/bar.png")))))))
