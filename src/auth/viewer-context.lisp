;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :auth/viewer-context
  (:use #:cl)
  (:export
   #:viewer-context-user
   #:viewer-context
   #:api-viewer-context))
(in-package :auth/viewer-context)

(defclass abstract-viewer-context ()
  ())

(defclass logged-in-viewer-context (abstract-viewer-context)
  ((user :initarg :user
         :reader viewer-context-user)))

(defclass api-viewer-context (logged-in-viewer-context)
  ())

(defclass viewer-context (logged-in-viewer-context)
  ())
