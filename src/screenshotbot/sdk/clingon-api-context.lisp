;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/sdk/clingon-api-context
  (:use #:cl)
  (:import-from #:easy-macros
                #:def-easy-macro)
  (:import-from #:clingon.options
                #:make-option)
  (:import-from #:clingon.command
                #:getopt)
  (:import-from #:screenshotbot/sdk/hostname
                #:api-hostname)
  (:import-from #:screenshotbot/sdk/api-context
                #:json-api-context
                #:api-context)
  (:import-from #:alexandria
                #:assoc-value)
  (:import-from #:util/health-check
                #:run-health-checks)
  (:import-from #:screenshotbot/sdk/install
                #:credential-file)
  (:import-from #:screenshotbot/api/model
                #:decode-json)
  (:import-from #:screenshotbot/sdk/fetch-run
                #:save-run)
  (:import-from #:util/request
                #:engine)
  (:import-from #:util/reused-ssl
                #:with-reused-ssl)
  (:export
   #:with-clingon-api-context))
(in-package :screenshotbot/sdk/clingon-api-context)

(defun make-api-context (&key api-key
                           api-secret
                           hostname
                           desktop)
  (cond
    (desktop
     (make-instance 'desktop-api-context))
    ((and (not (str:emptyp api-key))
          (not (str:emptyp api-secret)))
     (let ((key api-key)
           (secret api-secret))
       (when (str:emptyp key)
         (error "No --api-key provided"))
       (when(str:emptyp secret)
         (error "No --api-secret provided"))
       (let ((hostname (api-hostname
                        :hostname hostname)))
         (log:debug "Using hostname: ~a" hostname)
         (make-instance 'api-context
                        :key key
                        :secret secret
                        :hostname hostname))))
    ((path:-e (credential-file))
     (decode-json (uiop:read-file-string (credential-file))
                  'json-api-context))
    (t
     (error "You must provide a --api-key and --api-secret. (Alternatively, run `~~screenshotbot/recorder dev install` and follow the instructions to install a key."))))

(def-easy-macro with-clingon-api-context (&binding api-context cmd &fn fn)
  (let ((api-context (apply #'make-api-context
                            (loop for key in '(:api-key :api-secret :hostname :desktop)
                                  append `(,key ,(getopt cmd key))))))
    (with-reused-ssl ((engine api-context))
     (funcall fn api-context))))



