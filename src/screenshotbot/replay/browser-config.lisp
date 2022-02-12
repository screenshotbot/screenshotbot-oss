;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/replay/browser-config
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:dimensions
   #:height
   #:width
   #:browser-config
   #:browser-config-name
   #:browser-type
   #:mobile-emulation))
(in-package :screenshotbot/replay/browser-config)

(defclass dimensions ()
  ((height :initarg :height
           :reader height)
   (width :initarg :width
          :reader width)))

(defclass browser-config ()
  ((name :initarg :name
         :reader browser-config-name)
   (type :initarg :type
         :reader browser-type)
   (dimensions :initarg :dimensions
              :initform nil
              :reader dimensions)
   (mobile-emulation :initarg :mobile-emulation
                     :initform nil
                     :reader mobile-emulation)))
