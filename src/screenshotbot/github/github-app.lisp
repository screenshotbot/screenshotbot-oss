;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/github/github-app
  (:use #:cl)
  (:import-from #:bknr.datastore
                #:persistent-class
                #:store-object))
(in-package :screenshotbot/github/github-app)

(defclass abstract-github-app ()
  ())

(defclass transient-github-app ()
  ((app-id :initform nil
           :initarg :app-id
           :accessor github-app-id)
   (private-key :initform nil
                :initarg :private-key
                :accessor github-app-private-key)))

(defclass github-app (store-object
                      abstract-github-app)
  ((app-id :initform nil
           :initarg :app-id
           :accessor github-app-id)
   (private-key :initform nil
                :initarg :private-key
                :accessor github-app-private-key))
  (:metaclass persistent-class))


