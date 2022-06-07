;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/webdriver/test-screenshot
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/webdriver/screenshot
                #:decode-file-from-json-stream)
  (:local-nicknames (#:a #:alexandria)))
(in-package :screenshotbot/webdriver/test-screenshot)


(util/fiveam:def-suite)

(def-fixture state ()
  (let* ((input (asdf:system-relative-pathname :screenshotbot "fixture/rose.png"))
         (original-md5 (ironclad:byte-array-to-hex-string (md5:md5sum-file input))))
    (&body)))

(test simple-base64-decoding
  (with-fixture state ()
    (let* ((base64 (uiop:run-program
                   (list "base64" (namestring input))
                   :output 'string))
           (input (format nil "{\"data\":\"~a\"}" base64)))
      (uiop:with-temporary-file (:pathname output)
        (delete-file output)
        (let ((stream (make-string-input-stream input)))
          (decode-file-from-json-stream
           stream
           "data"
           :output output))
        (is (equal
             original-md5
             (ironclad:byte-array-to-hex-string (md5:md5sum-file output))))))))
