;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/json-mop
  (:use #:cl)
  (:import-from #:json-mop
                #:to-lisp-value
                #:to-json-value
                #:json-type)
  (:export
   #:json-mop-to-string
   #:ext-json-serializable-class))
(in-package :util/json-mop)

(defclass ext-json-serializable-class (json-mop:json-serializable-class)
  ())

(defmethod closer-mop:validate-superclass ((class ext-json-serializable-class)
                                           (super closer-mop:standard-class)) t)

(defmethod closer-mop:validate-superclass ((class standard-class)
                                           (super ext-json-serializable-class)) t)

(defclass ext-json-serializable-slot (json-mop::json-serializable-slot)
  ())

(defmethod closer-mop:direct-slot-definition-class ((class ext-json-serializable-class)
                                                    &rest initargs)
  (declare (ignore initargs))
  (find-class 'ext-json-serializable-slot))

(defclass nullable ()
  ((type :initarg :type
         :reader nullable-type)))

(defmethod json-type ((slot ext-json-serializable-slot))
  (let ((type (call-next-method)))
    (cond
      ((and
        (consp type)
        (eql 'or (first type))
        (eql 'null (second type)))
       (make-instance 'nullable :type (third type)))
      (t
       type))))

(defmethod to-json-value ((value null)
                          (type nullable))
  value)

(defmethod to-json-value (value
                          (type nullable))
  (to-json-value value (nullable-type type)))

(defmethod to-lisp-value ((value null)
                          (type nullable))
  nil)

(defmethod to-lisp-value (value
                          (type nullable))
  (to-lisp-value value (nullable-type type)))

;; Note that this changes the behavior of json-mop everywhere.. not
;; just when using with ext-json-serializable-class.
(defmethod to-json-value :around ((value null) (json-type cons))
  "Return the homogeneous sequence VALUE"
  (cond
    ((member (first json-type)
             '(:list :vector))
     #())
    (t
     (call-next-method))))

(defun json-mop-to-string (obj)
  (with-output-to-string (out)
    (yason:encode obj out)))
