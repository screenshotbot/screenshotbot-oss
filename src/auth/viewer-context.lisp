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
   #:api-viewer-context
   #:normal-viewer-context
   #:anonymous-viewer-context))
(in-package :auth/viewer-context)

(defclass abstract-viewer-context ()
  ()
  (:documentation "The same viewer might view the object through different contexts. For
example a viewer viewer via API or browser should have different
permissions. Or a super-admin might not want to browse with
super-admin priviledges by default."))

(defclass logged-in-viewer-context (abstract-viewer-context)
  ((user :initarg :user
         :reader viewer-context-user)))

(defclass api-viewer-context (logged-in-viewer-context)
  ())

(defclass normal-viewer-context (logged-in-viewer-context)
  ())

(defclass viewer-context (normal-viewer-context)
  ())

(defclass anonymous-viewer-context (abstract-viewer-context)
  ())
