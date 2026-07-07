;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/github/github-app
  (:use #:cl)
  (:import-from #:bknr.datastore
                #:persistent-class
                #:store-object)
  (:import-from #:util/store/store
                #:defindex)
  (:import-from #:util/store/fset-index
                #:fset-unique-index)
  (:export
   #:persisted-github-app-for-company))
(in-package :screenshotbot/github/github-app)

(defclass abstract-github-app ()
  ((app-name :initform nil
             :accessor %app-name)
   (installation-ids
    :initform (fset:empty-map)
    :accessor github-app-installation-ids
    :documentation "A cache: map from repo-id to installation-id")
   (last-cache-ts
    :initform 0
    :accessor last-cache-ts
    :documentation "The last time the installation-id was cleared")))

(defclass transient-github-app (abstract-github-app)
  ((app-id :initform nil
           :initarg :app-id
           :accessor github-app-id)
   (private-key :initform nil
                :initarg :private-key
                :accessor github-app-private-key)))

(defindex +company-index+
  'fset-unique-index
  :slot-name 'company)

(defclass github-app (store-object
                      abstract-github-app)
  ((app-id :initform nil
           :initarg :app-id
           :accessor github-app-id)
   (private-key :initform nil
                :initarg :private-key
                :accessor github-app-private-key)
   (company :initform nil
            :index +company-index+
            :initarg :company
            :accessor github-app-company
            :index-reader persisted-github-app-for-company))
  (:metaclass persistent-class))

(defgeneric fetch-github-app-name (github-app))

(defmethod github-app-name ((self abstract-github-app))
  (util:or-setf
   (%app-name self)
   (fetch-github-app-name self)))

(defmethod github-app-installation-ids :before ((self abstract-github-app))
  (let ((current-time (get-universal-time)))
    (when (< (last-cache-ts self) (- current-time (* 5 60)))
      (setf (slot-value self 'installation-ids) (fset:empty-map))
      (setf (last-cache-ts self) current-time))))
