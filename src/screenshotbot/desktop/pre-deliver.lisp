;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/desktop/pre-deliver
  (:use #:cl)
  (:export
   #:call-pre-delivery))
(in-package :screenshotbot/desktop/pre-deliver)

(defun call-pre-delivery ()
  (screenshotbot/magick/magick-lw:embed-magick-native))
