;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/webhook/model
  (:use #:cl)
  (:import-from #:bknr.datastore
                #:persistent-class
                #:store-object)
  (:import-from #:util/store/store
                #:with-class-validation)
  (:import-from #:util/json-mop
                #:ext-json-serializable-class))
(in-package :screenshotbot/webhook/model)

(with-class-validation
  (defclass webhook-event (store-object)
    ((%company :initarg :company
               :reader company)
     (event :initarg :event
            :reader event
            :documentation "The name of the event being sent. This will also be in the payload, but we use this for rendering information.")
     (%payload :initarg :payload
               :reader event-payload)
     (ts :initarg :ts
         :reader event-ts))
    (:metaclass persistent-class)
    (:default-initargs :ts (get-universal-time))))

(defclass webhook-payload ()
  ((event :initarg :event
          :json-key "event"
          :json-type :string
          :reader event))
  (:metaclass ext-json-serializable-class))
