;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :core/installation/installation
  (:use #:cl)
  (:import-from #:core/config/api
                #:validate
                #:config)
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

(defmethod installation-domain :around (installation)
  (let ((config-value (ignore-errors (config "installation.domain")))
        (installation-value (call-next-method)))

    (when (and
           config-value
           installation-value
           (not (equal "https://example.com" installation-value))
           (not (equal config-value installation-value)))
      (warn "Domain from config and installation does not match: "))
    (or
     config-value
     installation-value)))

(defvar *secondary-installations* nil
  "Non default installations, when allowing for multiple installations
per process.")

(defvar *installation*)

(pushnew '*installation* util/threading:*propagated-symbols*)

(defmethod site-alert (installation)
  nil)

(defmethod validate ((key (eql :installation.domain)) value)
  (unless (or
           (str:starts-with-p "http://" value)
           (str:starts-with-p "https://" value))
    (error "URL must be of the form https://... or http://..."))

  ;; validate parsing
  (let ((uri (quri:uri value)))
    (when (equal "/" (quri:uri-path uri))
      (error "Don't use the trailing / in the URI"))
    (unless (str:emptyp (quri:uri-path uri))
      (error "The URL must be a root URL, not ~a" (quri:uri-path uri)))))
