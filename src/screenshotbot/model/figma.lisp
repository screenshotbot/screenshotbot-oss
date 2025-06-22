;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/model/figma
  (:use #:cl)
  (:import-from #:bknr.datastore
                #:delete-object
                #:store-object
                #:persistent-class)
  (:import-from #:util/store/store
                #:defindex)
  (:import-from #:util/store/fset-index
                #:fset-unique-index)
  (:import-from #:bknr.indices
                #:index-get)
  (:import-from #:alexandria
                #:when-let)
  (:export
   #:figma-link-channel
   #:figma-link-screenshot-name
   #:figma-link-image
   #:figma-link
   #:figma-link-url))
(in-package :screenshotbot/model/figma)

(defindex +channel-screenshot-index+
  'fset-unique-index
  :slots '(channel screenshot-name))


(defclass figma-link (store-object)
  ((channel :initarg :channel
            :reader figma-link-channel)
   (screenshot-name :initarg :screenshot-name
                    :reader figma-link-screenshot-name)
   (image :initarg :image
          :reader figma-link-image)
   (url :initarg :url
        :reader figma-link-url))
  (:class-indices (channel-screenshot-name-index
                   :index +channel-screenshot-index+
                   :slots (channel screenshot-name)))
  (:metaclass persistent-class))


(defun find-existing-figma-link (&key channel screenshot-name)
  (index-get +channel-screenshot-index+
             (list channel screenshot-name)))

(defvar *lock* (bt:make-lock))

(defun update-figma-link (&rest args &key channel screenshot-name &allow-other-keys)
  (bt:with-lock-held (*lock*)
    (when-let ((existing (find-existing-figma-link :channel channel :screenshot-name screenshot-name)))
      (delete-object existing))
    (apply #'make-instance
           'figma-link
           args)))
