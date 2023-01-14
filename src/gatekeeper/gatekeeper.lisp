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
  (:export
   #:check))
(in-package :gatekeeper/gatekeeper)

(with-class-validation
  (defclass gatekeeper (store-object)
    ((name :initarg :name
           :reader gk-name
           :index-type unique-index
           :index-initargs (:test 'equal)
           :index-reader gk-with-name)
     (default-value :initform nil
                    :initarg :default
                    :accessor gk-default-value)
     (access-controls :initform nil
                      :accessor access-controls))
    (:metaclass persistent-class)))

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
  (let ((gk (gk-with-name (string name))))
    (cond
      (gk
       (loop for acl in (access-controls gk)
             if (member object (access-control-objects acl))
               return (eql :allow (acl-type acl))
             finally
                (return (gk-default-value gk))))
      (t
       default))))
