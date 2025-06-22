;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/figma/figma
  (:use #:cl)
  (:export
   #:figma))
(in-package :screenshotbot/figma/figma)

(defclass figma ()
  ((client-id :initarg :client-id
              :accessor figma-client-id
              :type string
              :documentation "The Figma OAuth client ID")
   (client-secret :initarg :client-secret
                  :accessor figma-client-secret
                  :type string
                  :documentation "The Figma OAuth client secret")))



