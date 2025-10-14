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

(defun api-hostname (&key (hostname (error "must provide :hostname")))
  (assert (str:non-empty-string-p hostname))
  (cond
    ((not (str:containsp "/" hostname))
     (format nil "https://~a" hostname))
    (t
     hostname)))


