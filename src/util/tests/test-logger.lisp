;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/tests/test-logger
  (:use #:cl
        #:fiveam)
  (:import-from #:util/logger
                #:format-log
                #:logger)
  (:import-from #:fiveam-matchers/strings
                #:contains-string)
  (:import-from #:fiveam-matchers/core
                #:assert-that))
(in-package :util/tests/test-logger)

(util/fiveam:def-suite)

(test simple-logger
  (uiop:with-temporary-file (:pathname p)
    (let ((logger (make-instance 'logger :file p)))
      (format-log logger
                  :info "hello world ~a" 1)
      (assert-that (uiop:read-file-string p)
                   (contains-string "hello world 1"))
      (format-log logger
                  :info "second")
      (assert-that (uiop:read-file-string p)
                   (contains-string (format nil "hello world 1~%")))
      (assert-that (uiop:read-file-string p)
                   (contains-string "second")))))

(test nil-logger
  (is (equal
       "hello world 1"
       (format-log nil
                   :info "hello world ~a" 1))))
