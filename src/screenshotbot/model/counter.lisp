;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/model/counter
  (:use #:cl)
  (:import-from #:screenshotbot/model/company
                #:company)
  (:import-from #:bknr.datastore
                #:deftransaction
                #:store-object
                #:persistent-class)
  (:import-from #:util/store/store
                #:defindex)
  (:import-from #:bknr.indices
                #:index-get
                #:hash-index
                #:unique-index)
  (:import-from #:util/store/fset-index
                #:fset-unique-index))
(in-package :screenshotbot/model/counter)

(defindex +counter-index+
  'fset-unique-index
  :slots '(%company %name))


(defclass counter (store-object)
  ((%company :initarg :company
             :reader counter-company)
   (%name :initarg :name
          :reader counter-name)
   (next-value :initarg :next-value
               :initform 1))
  (:class-indices (counter-index
                   :index +counter-index+))
  (:metaclass persistent-class))

(deftransaction tx-next-counter (company name)
  (let ((counter (or
                  ())))))

(defmacro defcounter (name ())
  (let* ((next-counter (intern (format nil "NEXT-~a" name)))
         (tx-next-counter (intern (format nil "%TX-~a" next-counter)))
         (index (intern (format nil "+~a-INDEX+" name))))
    `(progn
       (defindex ,index
         'unique-index
         :slot-name '%company)
       (defclass ,name (store-object)
         ((%company :initarg :company
                    :index ,index)
          (%next-value :initform 1
                       :accessor next-value))
         (:metaclass persistent-class))

       (deftransaction ,tx-next-counter (company)
         (tx-next-counter-impl ,index ',name company))

       (defun ,next-counter (company)
         (,tx-next-counter company)))))

(defun tx-next-counter-impl (index class-name company)
  (let ((counter (or
                  (index-get index company)
                  (make-instance class-name :company company))))
    (prog1
        (next-value counter)
      (incf (next-value counter)))))

(defmethod next-counter ((self company) name)
  "Get the next auto-increment counter for the company and the given name"
  (tx-next-counter self name))

