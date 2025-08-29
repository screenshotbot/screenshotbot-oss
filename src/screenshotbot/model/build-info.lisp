;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/model/build-info
  (:use #:cl)
  (:import-from #:bknr.datastore
                #:store-object
                #:persistent-class)
  (:import-from #:util/store/store
                #:with-class-validation
                #:defindex)
  (:import-from #:util/store/fset-index
                #:fset-unique-index
                #:fset-set-index)
  (:import-from #:bknr.indices
                #:index-get))
(in-package :screenshotbot/model/build-info)


(defindex +build-url-index-v2+
  'fset-unique-index
  :slots '(company build-url))

(unintern '+build-url-index+)

(defvar *lock* (bt:make-lock))


(with-class-validation
  (defclass build-info (store-object)
    ((build-url :initarg :build-url
               :reader build-url)
     (company :initarg :company)
     (repo-url :initarg :repo-url
               :accessor build-info-repo-url))
    (:metaclass persistent-class)
    (:class-indices (company-build-url-index
                     :index +build-url-index-v2+))
    (:documentation "On some CI systems (Xcode cloud!) we might need to store some
information across steps, because it might not be available in a
future step. This is just a way of storing that information.")))

(defun find-build-info (company build-url)
  (index-get +build-url-index-v2+ (list company build-url)))

(defun find-or-create-build-info (company build-url)
  (bt:with-lock-held (*lock*)
    (or
     (find-build-info company build-url)
     (make-instance 'build-info :build-url build-url :company company))))


