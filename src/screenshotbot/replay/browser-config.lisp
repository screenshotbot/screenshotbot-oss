;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/replay/browser-config
  (:use #:cl)
  (:import-from #:util/json-mop
                #:ext-json-serializable-class)
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
           :json-type :number
           :json-key "height"
           :reader height)
   (width :initarg :width
          :json-type :number
          :json-key "width"
          :reader width))
  (:metaclass ext-json-serializable-class))

(defclass browser-config ()
  ((name :initarg :name
         :json-type :string
         :json-key "name"
         :reader browser-config-name)
   (type :initarg :type
         :json-type :string
         :json-key "type"
         :reader browser-type)
   (dimensions :initarg :dimensions
               :json-type (or null dimensions)
               :json-key  "dimensions"
               :initform nil
               :reader dimensions)
   (mobile-emulation :initarg :mobile-emulation
                     :json-type (or null :string)
                     :json-key "mobileEmulation"
                     :initform nil
                     :reader mobile-emulation))
  (:metaclass ext-json-serializable-class))
