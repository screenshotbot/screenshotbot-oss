;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/tests/test-json-mop
  (:use #:cl
        #:fiveam)
  (:import-from #:util/json-mop
                #:ext-json-serializable-class)
  (:import-from #:fiveam-matchers/core
                #:assert-that)
  (:import-from #:fiveam-matchers/strings
                #:contains-string))
(in-package :util/tests/test-json-mop)


(util/fiveam:def-suite)

(defclass simple-obj ()
  ((arg1 :initarg :arg1
         :json-key "first"
         :json-type :string
         :accessor arg1)
   (arg2 :initarg :arg2
         :json-key "second"
         :json-type (or null :string)
         :accessor arg2))
  (:metaclass ext-json-serializable-class))

(defun %encode (obj)
  (with-output-to-string (out)
    (yason:encode obj out)))

(test simple-encoding
  (let ((obj (make-instance 'simple-obj :arg1 "foo")))
    (assert-that (%encode obj)
                 (contains-string "foo"))))

(test simple-encoding-with-null
  (let ((obj (make-instance 'simple-obj :arg1 "foo" :arg2 nil)))
    (assert-that (%encode obj)
                 (contains-string "foo"))))

(test simple-encoding-without-null
  (let ((obj (make-instance 'simple-obj :arg1 "foo" :arg2 "bleh")))
    (assert-that (%encode obj)
                 (contains-string "bleh"))
    (finishes
      (json-mop:json-to-clos (%encode obj)
                             'simple-obj))))
