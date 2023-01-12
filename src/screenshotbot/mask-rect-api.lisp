;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/mask-rect-api
  (:use #:cl)
  (:export
   #:mask-rect-left
   #:mask-rect-top
   #:mask-rect-width
   #:mask-rect-height))
(in-package :screenshotbot/mask-rect-api)

(defgeneric mask-rect-left (mask-rect))

(defgeneric mask-rect-top (mask-rect))

(defgeneric mask-rect-width (mask-rect))

(defgeneric mask-rect-height (mask-rect))
