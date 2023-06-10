;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/webhook/model
  (:use #:cl)
  (:import-from #:bknr.datastore
                #:delete-object
                #:persistent-class
                #:store-object)
  (:import-from #:util/store/store
                #:defindex
                #:with-class-validation)
  (:import-from #:util/json-mop
                #:ext-json-serializable-class)
  (:import-from #:util/store/fset-index
                #:fset-set-index
                #:fset-unique-index)
  (:import-from #:screenshotbot/model/company
                #:company
                #:company-with-name)
  (:import-from #:util/store/object-id
                #:find-by-oid)
  (:import-from #:alexandria
                #:when-let)
  (:import-from #:screenshotbot/model/core
                #:generate-api-key)
  (:import-from #:screenshotbot/audit-log
                #:base-audit-log)
  (:export
   #:signing-key
   #:update-config
   #:webhook-event))
(in-package :screenshotbot/webhook/model)

(defvar *lock* (bt:make-lock))

(defindex +company-event-index+ 'fset-set-index :slot-name '%company)

(with-class-validation
  (defclass webhook-event (base-audit-log)
    ((event :initarg :event
            :reader event
            :documentation "The name of the event being sent. This will also be in the payload, but we use this for rendering information.")
     (%payload :initarg :payload
               :reader event-payload))
    (:metaclass persistent-class)))

(defindex +company-index+ 'fset-unique-index :slot-name '%company)


(with-class-validation
  (defclass webhook-company-config (store-object)
    ((%company :initarg :company
               :reader config-company
               :index +company-index+
               :index-reader webhook-config-for-company)
     (endpoint :initarg :endpoint
               :accessor endpoint
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

(defmethod ensure-webhook-config ((company company))
  (bt:with-lock-held (*lock*)
    (let ((config
            (webhook-config-for-company company)))
      (or
       config
       (make-instance 'webhook-company-config
                      :company company
                      :endpoint ""
                      :enabledp nil
                      :signing-key (generate-api-key))))))

(defclass webhook-payload ()
  ((event :initarg :event
          :json-key "event"
          :json-type :string
          :reader event))
  (:metaclass ext-json-serializable-class))

(defun update-config (&key company endpoint signing-key enabled)
  (bt:with-lock-held (*lock*)
    (when-let ((prev (webhook-config-for-company company)))
      (delete-object prev))
    (make-instance 'webhook-company-config
                   :company company
                   :endpoint endpoint
                   :signing-key signing-key
                   :enabledp enabled)
    (hex:safe-redirect "/settings/webhook")))
