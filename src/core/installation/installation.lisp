;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :core/installation/installation
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:abstract-installation
   #:*installation*
   #:installation-domain
   #:installation-name))
(in-package :core/installation/installation)

(defclass abstract-installation ()
  ((domain :initarg :domain
           :initform "https://example.com"
           :reader installation-domain)
   (name :initform :name
         :initarg :name
         :reader installation-name
         :documentation "A symbol representing an installation name. This
name will be used from data to represent which installation it belongs to.")))

(defmethod installation-domain ((self null))
  nil)

(defvar *secondary-installations* nil
  "Non default installations, when allowing for multiple installations
per process.")

(defvar *installation*)

(pushnew '*installation* util/threading:*propagated-symbols*)

(defmethod site-alert (installation)
  nil)
