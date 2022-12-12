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
   #:installation-domain))
(in-package :core/installation/installation)

(util/migrations:ensure-symbol-in-package
 #:*installation*
 :old #:screenshotbot/installation
 :new #:core/installation/installation)
(export '*installation*)

(util/migrations:ensure-symbol-in-package
 #:domain
 :old #:screenshotbot/installation
 :new #:core/installation/installation)

(defclass abstract-installation ()
  ((domain :initarg :domain
           :initform "https://example.com"
           :reader installation-domain)))

(defvar *installation*)
