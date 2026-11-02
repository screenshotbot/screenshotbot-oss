;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/api/crash
  (:use #:cl)
  (:import-from #:screenshotbot/server
                #:defhandler))
(in-package :screenshotbot/api/crash)

(defun forward-crash (event)
  (setf (hunchentoot:header-out :content-type) "application/json; charset=utf-8")
  (cond
    ((= 0 (length event))
     (setf (hunchentoot:return-code*) hunchentoot:+http-bad-request+)
     "{\"success\":false,\"error\":\"Expected a Sentry event in the request body\"}")
    (t
     (sentry-client:forward-event event)
     "{\"success\":true}")))

;; This is deliberately not a DEFAPI, since this does not need an API
;; key.
(defhandler (%post-crash :uri "/api/crash" :method :post) ()
  ;; We keep this binary to forward exactly what we were sent, without
  ;; a round trip through an external format.
  (forward-crash (hunchentoot:raw-post-data :force-binary t)))
