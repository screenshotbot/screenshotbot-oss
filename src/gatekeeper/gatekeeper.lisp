;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :gatekeeper/gatekeeper
  (:nicknames :gk)
  (:use #:cl)
  (:import-from #:util/store
                #:with-class-validation)
  (:import-from #:bknr.datastore
                #:store-object)
  (:import-from #:bknr.datastore
                #:persistent-class)
  (:import-from #:bknr.indices
                #:unique-index)
  (:import-from #:bknr.datastore
                #:with-transaction)
  (:import-from #:bknr.datastore
                #:class-instances)
  (:import-from #:util/store/fset-index
                #:fset-unique-index)
  (:export
   #:check
   #:create
   #:show
   #:insert-acl
   #:enable
   #:disable
   #:normalize-object
   #:allow
   #:deny
   #:show-all))
(in-package :gatekeeper/gatekeeper)

(with-class-validation
  (defclass gatekeeper (store-object)
    ((name :initarg :name
           :reader gk-name
           :index-type fset-unique-index
           :index-reader %gk-with-name
           :index-values all-gks)
     (default-value :initform nil
                    :initarg :default
                    :accessor gk-default-value)
     (access-controls :initform nil
                      :accessor access-controls))
    (:metaclass persistent-class)))

(defun gk-with-name (name)
  (%gk-with-name (string name)))

(defun gk-with-name! (name)
  (let ((ret (gk-with-name name)))
    (assert ret)
    ret))

(with-class-validation
  (defclass access-control (store-object)
    ((type :initarg :type
           :documentation "Either :allow or :deny"
           :accessor acl-type)
     (objects :initarg :objects
              :accessor access-control-objects
              :documentation "All the objects on which this ACL applies"))
    (:metaclass persistent-class)))

(defun check (name object &key default)
  (let ((gk (gk-with-name name)))
    (cond
      (gk
       (loop for acl in (access-controls gk)
             if (member object (access-control-objects acl))
               return (eql :allow (acl-type acl))
             finally
                (return (gk-default-value gk))))
      (t
       default))))

(defun create (name &key default)
  (make-instance
   'gatekeeper :name (string name) :default default))

(defun enable (name &key (default t))
  (let ((gk (gk-with-name! name)))
   (with-transaction ()
     (setf (gk-default-value gk) default))))

(defun disable (name)
  (enable name :default nil))

(defun show-gk (gk)
  (cond
    (gk
     (format t "~a~%" (gk-name gk))
     (format t "  Default value: ~a~%" (gk-default-value gk))
     (format t "  ~a ACL items~%" (length (access-controls gk)))
     (loop for acl in (access-controls gk)
           do
              (format t "  | ~a: ~s~%"
                      (string (acl-type acl))
                      (access-control-objects acl))))
    (t
     (log:info "No such gk"))))

(defun show (name)
  (let ((gk (gk-with-name (string name))))
    (show-gk gk)))

(defun show-all ()
  (loop for gk in (class-instances 'gatekeeper)
        if (ignore-errors (gk-name gk))
        do (show-gk gk)))


(defmethod normalize-object (obj)
  obj)

(defmethod normalize-object :around (obj)
  (let ((ret (call-next-method)))
    (when (or (null ret) (keywordp ret))
      (error "~a could not be normalized" ret))
    ret))

(defun push-acl (name type obj)
  (let ((gk (gk-with-name! name)))
    (let ((acl (make-instance 'access-control
                              :type type
                              :objects (list (normalize-object obj)))))
      (with-transaction ()

        (push acl (access-controls gk))))))

(defun allow (name obj)
  (push-acl name :allow obj))

(defun deny (name obj)
  (push-acl name :deny obj))
