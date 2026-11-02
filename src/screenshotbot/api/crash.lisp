;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/api/crash
  (:use #:cl)
  (:import-from #:screenshotbot/api/core
                #:api-error
                #:authenticate-api-request)
  (:import-from #:screenshotbot/server
                #:defhandler)
  (:import-from #:util/throttler
                #:ip-throttler
                #:throttle!))
(in-package :screenshotbot/api/crash)

(defvar *throttler* (make-instance 'ip-throttler
                                   :name "api-crash"
                                   :tokens 100)
  "Throttles the requests that aren't authenticated.")

(defun authenticated-p ()
  "Whether the request has valid credentials. Invalid credentials are
treated the same as no credentials."
  (when (hunchentoot:header-in* :authorization)
    (handler-case
        (authenticate-api-request hunchentoot:*request*)
      (api-error ()
        nil))))

(defun forward-crash (event)
  (setf (hunchentoot:header-out :content-type) "application/json; charset=utf-8")
  (cond
    ((= 0 (length event))
     (setf (hunchentoot:return-code*) hunchentoot:+http-bad-request+)
     "{\"success\":false,\"error\":\"Expected a Sentry event in the request body\"}")
    (t
     (sentry-client:forward-event event)
     "{\"success\":true}")))

;; This is not a DEFAPI, since a crash can be reported without an API
;; key.
(defhandler (%post-crash :uri "/api/crash" :method :post) ()
  (unless (authenticated-p)
    (throttle! *throttler*))
  ;; We keep this binary to forward exactly what we were sent, without
  ;; a round trip through an external format.
  (forward-crash (hunchentoot:raw-post-data :force-binary t)))
