;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/model/figma
  (:use #:cl)
  (:import-from #:bknr.datastore
                #:store-object
                #:persistent-class)
  (:export
   #:figma-link-channel
   #:figma-link-screenshot-name
   #:figma-link-image))
(in-package :screenshotbot/model/figma)

(defclass figma-link (store-object)
  ((channel :initarg :channel
            :reader figma-link-channel)
   (screenshot-name :initarg :screenshot-name
                    :reader figma-link-screenshot-name)
   (image :initarg :image
          :reader figma-link-image))
  (:metaclass persistent-class))
