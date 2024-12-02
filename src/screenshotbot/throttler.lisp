;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/throttler
  (:use #:cl)
  (:import-from #:util/throttler
                #:throttled-error
                #:throttle!
                #:ip-throttler)
  (:export
   #:global-throttler))
(in-package :screenshotbot/throttler)

(defparameter *global-request-throttler*
  (make-instance 'ip-throttler
                 :tokens 360000))

(defvar *image-request-throttler*
  (make-instance 'ip-throttler
                 :tokens 72000))

(defun image-script-p (script-name)
  "We don't want to throttle image endpoints by the same level, since we
might have a burst of requests coming in per run"
  (or
   (str:starts-with-p "/api/image"
                      script-name)
   (str:starts-with-p "/image"
                      script-name)))

(defmethod global-throttler (installation)
  "We want to be able to change the global throttler for enterprise
installs."
  *global-request-throttler*)

(defmethod maybe-throttle-request (installation request)
  (handler-case
      (let ((script-name (hunchentoot:script-name request)))
        (cond
          ((image-script-p script-name)
           (throttle! *image-request-throttler*))
          (t
           (throttle! *global-request-throttler*)))
        nil)
    (throttled-error (e)
      (setf (hunchentoot:return-code*) 429)
      (warn "Too many global requests")
      "Too Many Requests")))


