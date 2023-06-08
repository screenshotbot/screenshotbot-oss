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
                #:defindex
                #:with-class-validation)
  (:import-from #:util/json-mop
                #:ext-json-serializable-class)
  (:import-from #:util/store/fset-index
                #:fset-unique-index)
  (:import-from #:screenshotbot/model/company
                #:company-with-name)
  (:import-from #:util/store/object-id
                #:find-by-oid)
  (:export
   #:signing-key))
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

(defindex +company-index+ 'fset-unique-index :slot-name '%company)

(with-class-validation
  (defclass webhook-company-config (store-object)
    ((%company :initarg :company
               :reader reader
               :index +company-index+
               :index-reader webhook-config-for-company)
     (endpoint :initarg :endpoint
               :reader endpoint
               :initform "https://tdrhq.com/sb-webhook")
     (signing-key
      :initarg :signing-key
      :reader signing-key
      :initform "foobarCardar")
     (enabledp
      :initarg :enabledp
      :reader enabledp))
    (:metaclass persistent-class)))

#+nil
(make-instance 'webhook-company-config
               :company (find-by-oid "5fd16bcf4f4b3822fd0000e1"))

(defclass webhook-payload ()
  ((event :initarg :event
          :json-key "event"
          :json-type :string
          :reader event))
  (:metaclass ext-json-serializable-class))
