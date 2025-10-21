;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/sdk/version-check
  (:use #:cl)
  (:import-from #:screenshotbot/api/model
                #:decode-json
                #:version-number
                #:version
                #:*api-version*)
  (:import-from #:easy-macros
                #:def-easy-macro)
  (:import-from #:util/request
                #:http-request)
  (:import-from #:util/health-check
                #:def-health-check)
  (:import-from #:screenshotbot/sdk/backoff
                #:maybe-retry-request
                #:backoff)
  (:import-from #:screenshotbot/sdk/hostname
                #:format-api-url)
  (:import-from #:screenshotbot/sdk/api-context
                #:fetch-version
                #:remote-version)
  (:local-nicknames (#:a #:alexandria)
                    (#:api-context #:screenshotbot/sdk/api-context))
  (:export
   #:*client-version*
   #:remote-supports-put-run
   #:fetch-version))
(in-package :screenshotbot/sdk/version-check)

(defparameter *client-version* (asdf:system-version
                                (asdf:find-system :screenshotbot.sdk/library))
  "The client version. Note that is different from the *api-version*.")

(defun remote-supports-basic-auth-p (api-context)
  "Prior to this version, the auth was passed as http parameters. That
wasn't great for security since it might mean the plain-text secret
might get logged in the webserver logs."
  (>= (remote-version api-context) 2))

(defun remote-supports-put-run (api-context)
  (>= (remote-version api-context) 4))

(defvar *in-here* nil)
(auto-restart:with-auto-restart (:attempt attempt)
  (defmethod fetch-version (api-context)
    (log:info "Fetching remote version")
    (assert (not *in-here*))
    (let ((*in-here* t))
     (multiple-value-bind (body ret)
         (http-request
          (format-api-url api-context "/api/version")
          :basic-authorization (unless (and
                                        (str:emptyp (api-context:key api-context))
                                        (str:emptyp (api-context:secret api-context)))
                                   (list
                                    (api-context:key api-context)
                                    (api-context:secret api-context)))
          :want-string t
          :engine (api-context:engine api-context))
       (maybe-retry-request
        ret
        :attempt attempt
        :restart 'retry-fetch-version)
       (let ((version (cond
                        ((eql 200 ret)
                         (decode-json body 'version))
                        ((eql 404 ret)
                         (log:warn "/api/version responded 404, this is probably because of running an old version of OSS Screenshotbot service")
                         (make-instance 'version :version 1))
                        (t
                         (log:error "/api/version failed with code ~a" ret)
                         (error "/api/version failed")))))
         (warn-for-bad-versions (version-number version))
         (values
          (version-number version)
          version))))))

(defun bad-version-p (remote-version)
  (>
   (abs
    (- remote-version *api-version*))
   1))

(defun warn-for-bad-versions (remote-version)
  (when (not (bad-version-p remote-version))
    (return-from warn-for-bad-versions nil))
  (flet ((warning (reason)
           (log:warn "Server is running API version ~a, but this client uses version ~a. This is most likely supported, however, it's more likely to have bugs. ~a" remote-version *api-version* reason)))
    (cond
      ((> *api-version* remote-version)
       (warning "If you're using OSS Screenshotbot, we suggest upgrading."))
      ((< *api-version* remote-version)
       (warning "Consider upgrading the CLI tool.")))))

(def-health-check verify-can-decode-json ()
  (eql 10 (version-number (decode-json "{\"version\":10}" 'version))))
