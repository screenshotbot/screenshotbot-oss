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
  (:import-from #:util/digests
                #:md5-file)
  (:local-nicknames (#:a #:alexandria)))
(in-package :screenshotbot/webdriver/test-screenshot)


(util/fiveam:def-suite)

(def-fixture state ()
  (let* ((input-file (asdf:system-relative-pathname :screenshotbot "fixture/rose.png"))
         (original-md5 (ironclad:byte-array-to-hex-string (md5-file input-file))))
    (&body)))

(defun read-file-bytes (input-file)
  (flexi-streams:with-output-to-sequence (output)
    (with-open-file (input input-file :element-type '(unsigned-byte 8))
      (uiop:copy-stream-to-stream input output :element-type '(unsigned-byte 8)))))

#-ccl
(test simple-base64-decoding
  (with-fixture state ()
    (let* ((binary (read-file-bytes input-file))
           (base64 (base64:usb8-array-to-base64-string binary))
           (input (format nil "{\"data\":\"~a\"}" base64)))
      (is (eql 6975 (length binary)))
      (is (eql (* 4/3 6975) (length base64)))
      (uiop:with-temporary-file (:pathname output)
        (delete-file output)
        (let ((stream (make-string-input-stream input)))
          (decode-file-from-json-stream
           stream
           "data"
           :output output))
        (is (path:-e output))
        (is (equal (length binary) (trivial-file-size:file-size-in-octets input-file)))
        (loop for e1 across (read-file-bytes input-file)
              for e2 across (read-file-bytes output)
              for i from 0
              if (not (eql e1 e2))
                do (Fail "At position ~a: expected ~a, got ~a" i e1 e2)
                (return))
        (is (Equalp (read-file-bytes input-file)
                    (read-file-bytes output)))
        (is (equal (trivial-file-size:file-size-in-octets input-file)
                   (trivial-file-size:file-size-in-octets output)))
        (is (equal
             original-md5
             (ironclad:byte-array-to-hex-string (md5:md5sum-file output))))))))
