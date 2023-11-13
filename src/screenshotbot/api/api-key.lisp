;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/api/api-key
  (:use #:cl)
  (:import-from #:screenshotbot/api/core
                #:*api-key*
                #:defapi)
  (:import-from #:screenshotbot/model/api-key
                #:expires-at
                #:api-key-description
                #:cli-api-key))
(in-package :screenshotbot/api/api-key)

(defapi (register-cli-api-key :uri "/api/finalize-cli" :method :post) (hostname)
  "Finalized a CLI-API-KEY, i.e. it marks it as a key in use, that never expires"
  (cond
    ((and
      *api-key*
      (typep *api-key* 'cli-api-key))
     (setf (expires-at *api-key*) nil)
     (when (and
            (str:emptyp (api-key-description *api-key*))
            (not (str:emptyp hostname)))
       (setf (api-key-description *api-key*)
             (format nil "CLI use on ~a" hostname)))
     "OK")
    (t
     (error "This API key has expired, try again"))))
