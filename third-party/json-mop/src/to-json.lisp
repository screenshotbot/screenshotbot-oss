;; Copyright (c) 2015 Grim Schjetne
;;
;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

(in-package #:json-mop)

(defvar *encode-unbound-slots* nil)

(defgeneric to-json-value (value json-type)
  (:documentation
   "Turns a VALUE into a form appropriate for consumption by Yason"))

(defmethod to-json-value (value (json-type (eql :any)))
  "When the JSON type is :ANY, Pass the VALUE unchanged"
  value)

(defmethod to-json-value ((value null) json-type)
  (error 'null-value :json-type json-type))

(defmethod to-json-value ((value string) (json-type (eql :string)))
  "Return the string VALUE"
  value)

(defmethod to-json-value ((value number) (json-type (eql :number)))
  "Return the number VALUE"
  value)

(defmethod to-json-value ((value hash-table) (json-type (eql :hash-table)))
  "Return the hash-table VALUE"
  value)

(defmethod to-json-value ((value vector) (json-type (eql :vector)))
  "Return the vector VALUE"
  value)

(defmethod to-json-value ((value list) (json-type (eql :list)))
  "Return the list VALUE"
  value)

(defmethod to-json-value ((value null) (json-type (eql :list)))
  "Return the empty list VALUE"
  #())

(defmethod to-json-value (value (json-type (eql :bool)))
  "Return the boolean true"
  'true)

(defmethod to-json-value ((value null) (json-type (eql :bool)))
  "Return the boolean false"
  'false)

(defmethod to-json-value ((value sequence) (json-type cons))
  "Return the homogeneous sequence VALUE"
  (ecase (first json-type)
    (:list (check-type value list))
    (:vector (check-type value vector)))
  (make-instance 'homogeneous-sequence-intermediate-class
                 :values value
                 :sequence-json-type (first json-type)
                 :element-json-type (second json-type)))

(defclass homogeneous-sequence-intermediate-class ()
  ((values :initarg :values)
   (sequence-json-type :initarg :sequence-json-type)
   (element-json-type :initarg :element-json-type)))

(defmethod to-json-value (value (json-type symbol))
  (if (eql (class-of value) (find-class json-type))
      value
      (error 'json-type-error :json-type json-type)))

(defmethod encode ((sequence homogeneous-sequence-intermediate-class)
                   &optional (stream *standard-output*))
  (with-output (stream)
    (with-array ()
      (with-slots (values sequence-json-type element-json-type) sequence
        (map nil (lambda (element)
                   (handler-case
                       (encode-array-element (to-json-value element element-json-type))
                     (null-value (condition)
                       (declare (ignore condition))
                       (restart-case (error 'null-in-homogeneous-sequence
                                            :json-type (list sequence-json-type
                                                             element-json-type))
                         (use-value (value)
                           :report "Specify a value to use in place of the null"
                           :interactive read-eval-query
                           (encode-array-element value))))))
             values))))
  sequence)

(defmethod encode ((object json-serializable)
                   &optional (stream *standard-output*))
  (with-output (stream)
    (with-object ()
      (loop for class in (closer-mop:class-precedence-list (class-of object))
         do (loop for slot in (closer-mop:class-direct-slots class)
               when (typep slot 'json-serializable-slot)
               do (awhen (json-key-name slot)
                    (handler-case
                        (encode-object-element
                         it
                         (to-json-value
                          (slot-value object (closer-mop:slot-definition-name slot))
                          (json-type slot)))
                      (unbound-slot (condition)
                        (declare (ignore condition))
                        (when *encode-unbound-slots*
                          (encode-object-element it nil)))))))))
  object)

