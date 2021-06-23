;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(pkg:define-package :screenshotbot/model/test-acceptable
  (:use #:cl
        #:alexandria
        #:fiveam
        #:./report))

(def-suite* :screenshotbot/model/test-acceptable)

(test simple-acceptable-set-get
  (let ((a (make-instance 'base-acceptable)))
    (is (equal nil (acceptable-state a)))
    (setf (acceptable-state a) :accepted)
    (is (equal :accepted (acceptable-state a)))
    (signals error
      (setf (acceptable-state a) :foo))))
