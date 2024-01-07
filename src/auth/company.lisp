;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :auth/company
  (:use #:cl
        #:auth))
(in-package :auth/company)

(defun (setf current-company) (company)
  (can-view! company)
  (setf (auth:session-value :company) company
        (auth:request-account hunchentoot:*request*) company))

(defun current-company ()
  (and
   (boundp 'hunchentoot:*request*)
   (auth:request-account hunchentoot:*request*)))
