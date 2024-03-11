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

(defclass json-serializable-class (closer-mop:standard-class) ())

(defmethod closer-mop:validate-superclass ((class json-serializable-class)
                                           (super closer-mop:standard-class)) t)

(defmethod closer-mop:validate-superclass ((class standard-class)
                                           (super json-serializable-class)) t)

(defclass json-serializable-slot (closer-mop:standard-direct-slot-definition)
  ((json-key :initarg :json-key
             :initform nil
             :reader json-key-name)
   (json-type :initarg :json-type
              :initform :any
              :reader json-type)))

(defmethod json-key-name ((slot closer-mop:standard-direct-slot-definition))
  nil)

(defmethod closer-mop:direct-slot-definition-class ((class json-serializable-class)
                                                    &rest initargs)
  (declare (ignore class initargs))
  (find-class 'json-serializable-slot))

(defclass json-serializable () ())

(defmethod initialize-instance :around ((class json-serializable-class)
                                        &rest rest &key direct-superclasses)
  (apply #'call-next-method
         class
         :direct-superclasses
         (append direct-superclasses (list (find-class 'json-serializable)))
         rest))

(defmethod reinitialize-instance :around ((class json-serializable-class)
                                          &rest rest &key direct-superclasses)
  (apply #'call-next-method
         class
         :direct-superclasses
         (append direct-superclasses (list (find-class 'json-serializable)))
         rest))
