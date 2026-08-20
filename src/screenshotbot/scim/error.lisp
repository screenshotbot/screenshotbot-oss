;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/scim/error
  (:use #:cl)
  (:export
   #:api-error-code
   #:api-error-type
   #:api-error-reason))
(in-package :screenshotbot/scim/error)


(define-condition api-error (error)
  ((code :initarg :code
         :reader api-error-code)
   (scim-type :initarg :type
              :initform nil
              :reader api-error-type)
   (reason :initarg :reason
           :initform "NA"
           :reader api-error-reason)))

(defmethod print-object ((self api-error) output)
  (format output "API-ERROR: ~a"
          (api-error-reason self)))


