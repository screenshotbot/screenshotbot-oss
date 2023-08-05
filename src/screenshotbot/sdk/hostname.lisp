;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/sdk/hostname
  (:use #:cl)
  (:local-nicknames (#:flags #:screenshotbot/sdk/flags))
  (:export
   #:api-hostname
   #:format-api-url))
(in-package :screenshotbot/sdk/hostname)

(defun api-hostname ()
  (cond
    ((and flags:*desktop*
          (not
           (str:starts-with-p "http://localhost" flags:*hostname*)))
     "http://localhost:4095")
    ((not (str:containsp "/" flags:*hostname*))
     (format nil "https://~a" flags:*hostname*))
    (t
     flags:*hostname*)))


(defun format-api-url (api)
  (quri:render-uri
   (quri:merge-uris
    api
    (api-hostname))))
