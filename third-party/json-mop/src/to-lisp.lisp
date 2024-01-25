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

(defgeneric to-lisp-value (value json-type)
  (:documentation
   "Turns a value passed by Yason into the appropriate
  Lisp type as specified by JSON-TYPE"))

(defmethod to-lisp-value ((value (eql :null)) json-type)
  "When the value is JSON null, signal NULL-VALUE error"
  (error 'null-value :json-type json-type))

(defmethod to-lisp-value (value (json-type (eql :any)))
  "When the JSON type is :ANY, Pass the VALUE unchanged"
  value)

(defmethod to-lisp-value ((value string) (json-type (eql :string)))
  "Return the string VALUE"
  value)

(defmethod to-lisp-value ((value number) (json-type (eql :number)))
  "Return the number VALUE"
  value)

(defmethod to-lisp-value ((value hash-table) (json-type (eql :hash-table)))
  "Return the hash-table VALUE"
  value)

(defmethod to-lisp-value ((value vector) (json-type (eql :vector)))
  "Return the vector VALUE"
  value)

(defmethod to-lisp-value ((value vector) (json-type (eql :list)))
  "Return the list VALUE"
  (coerce value 'list))

(defmethod to-lisp-value (value (json-type (eql :bool)))
  "Return the boolean VALUE"
  (ecase value (true t) (false nil)))

(defmethod to-lisp-value ((value vector) (json-type cons))
  "Return the homogeneous sequence VALUE"
  (map (ecase (first json-type)
         (:vector 'vector)
         (:list 'list))
       (lambda (item)
         (handler-case (to-lisp-value item (second json-type))
           (null-value (condition)
             (declare (ignore condition))
             (restart-case (error 'null-in-homogenous-sequence
                                  :json-type json-type)
               (use-value (value)
                 :report "Specify a value to use in place of the null"
                 :interactive read-eval-query
                 value)))))
       value))

(defmethod to-lisp-value ((value hash-table) (json-type symbol))
  "Return the CLOS object VALUE"
  (json-to-clos value json-type))

(defgeneric json-to-clos (input class &rest initargs))

(defmethod json-to-clos ((input hash-table) class &rest initargs)
  (let ((lisp-object (apply #'make-instance class initargs))
        (key-count 0))
    (loop for slot in (closer-mop:class-direct-slots (find-class class))
          do (awhen (json-key-name slot)
               (handler-case
                   (progn
                     (setf (slot-value lisp-object
                                       (closer-mop:slot-definition-name slot))
                           (to-lisp-value (gethash it input :null)
                                          (json-type slot)))
                     (incf key-count))
                 (null-value (condition)
                   (declare (ignore condition)) nil))))
    #+nil ;; Arnold disabled this warning,
    (when (zerop key-count) (warn 'no-values-parsed
                                  :hash-table input
                                  :class-name class))
    (values lisp-object key-count)))

(defmethod json-to-clos ((input stream) class &rest initargs)
  (apply #'json-to-clos
         (parse input
                      :object-as :hash-table
                      :json-arrays-as-vectors t
                      :json-booleans-as-symbols t
                      :json-nulls-as-keyword t)
         class initargs))

(defmethod json-to-clos ((input pathname) class &rest initargs)
  (with-open-file (stream input)
    (apply #'json-to-clos stream class initargs)))

(defmethod json-to-clos ((input string) class &rest initargs)
  (apply #'json-to-clos (make-string-input-stream input) class initargs))
