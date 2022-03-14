;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/plan
  (:use #:cl)
  (:import-from #:screenshotbot/user-api
                #:current-company
                #:current-user
                #:user)
  (:import-from #:screenshotbot/installation
                #:installation)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:plan))
(in-package :screenshotbot/plan)

(defclass plan ()
  ()
  (:documentation "base class for all plans. Default OSS plan"))

(defvar *plan* (make-instance 'plan))

(defmethod company-plan (company installation)
  "Get the plan for the current user"
  *plan*)

(defun plan ()
  (user-plan
   (current-company)
   (installation)))
