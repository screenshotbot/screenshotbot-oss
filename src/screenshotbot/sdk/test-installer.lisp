;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

;;;; This tests the installer.sh file. :/ So technically not a Lisp test.

(defpackage :screenshotbot/sdk/test-installer
  (:use #:cl
        #:fiveam)
  (:import-from #:fiveam-matchers/core
                #:does-not
                #:assert-that)
  (:import-from #:fiveam-matchers/strings
                #:contains-string))
(in-package :screenshotbot/sdk/test-installer)

(util/fiveam:def-suite)

(def-fixture state ()
  (tmpdir:with-tmpdir (dir)
    (tmpdir:with-tmpdir (installer-dir)
      (uiop:copy-file (asdf:system-relative-pathname :screenshotbot.sdk
                                                     "installer.sh")
                      (path:catfile installer-dir "installer.sh"))
      (flet ((write-file (file content)
               (with-open-file (stream (path:catfile installer-dir file) :direction :output)
                 (write-string content stream))))
        (write-file "recorder" "")
        (write-file "sdk" "")
        (when (uiop:os-macosx-p)
         (write-file "sdk.lwheap" "")))
      (&body))))

(test simple-installer-test
  (with-fixture state ()
    (multiple-value-bind (output error-output)
        (uiop:run-program
            (list "bash" "-c"
                  (format nil
                          "cd ~a && SCREENSHOTBOT_DIR='~a' sh ./installer.sh"
                          (pathname installer-dir)
                          (pathname dir)))
            :output 'string
            :error-output 'string)
      (assert-that output
                   (does-not (contains-string "No such file")))
      (assert-that error-output
                   (does-not (contains-string "No such file")))
      (is (path:-e (path:catfile dir "recorder")))
      (when (uiop:os-macosx-p)
        (is (path:-e (path:catfile dir "recorder.lwheap")))))))
