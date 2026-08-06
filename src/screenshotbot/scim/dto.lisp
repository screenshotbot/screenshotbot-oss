;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/scim/dto
  (:use #:cl)
  (:import-from #:util/json-mop
                #:ext-json-serializable-class))
(in-package :screenshotbot/scim/dto)

(defclass external-name ()
  ((formatted :json-type :string
              :json-key "formatted"
              :reader name-formatted)
   (given :json-type :string
          :json-key "givenName"
          :reader name-given)
   (family-name :json-type :string
                :json-key "familyName"
                :reader name-family-name))
  (:metaclass ext-json-serializable-class))

(defclass external-email ()
  ((value :initarg :value
          :reader external-email-value
          :json-type :string
          :json-key "value")
   (type :initarg :type
         :reader external-email-typ
         :json-type :string
         :json-key "type")
   (primary :initarg :primary
            :reader external-email-primary
            :json-type :bool
            :json-key "primary"))
  (:metaclass ext-json-serializable-class))

(defclass external-user ()
  ((schemas :initform '("urn:ietf:params:scim:schemas:core:2.0:User")
            :json-type (:list :string)
            :json-key "schemas")
   (user-name :json-type :string
              :json-key "userName"
              :initarg :user-name
              :reader external-user-user-name)
   (name :json-type external-name
         :initarg :name
         :json-key "name"
         :reader external-user-name)
   (id :json-type :string
       :initarg :id
       :json-key "id")
   (external-id :json-type :string
                :initarg :external-id
                :json-key "externalId"
                :reader external-user-external-id)
   (emails :json-type (:list external-email)
           :initform nil
           :initarg :emails
           :json-key "emails"
           :reader external-user-emails))
  (:metaclass ext-json-serializable-class))

(defmethod initialize-instance :after ((self external-user) &key external-id &allow-other-keys)
  (declare (optimize (debug 3) (speed 0)))
  (unless external-id
    (slot-makunbound self 'external-id)))


(defclass list-response ()
  ((schemas :initform '("urn:ietf:params:scim:api:messages:2.0:ListResponse")
            :json-type (:list :string)
            :json-key "schemas")
   (total-results :json-type :number
                  :json-key "totalResults"
                  :initarg :total-results)
   (start-index :json-type :number
                :initarg :start-index
                :json-key "startIndex")
   (items-per-page :json-type :number
                   :initarg :items-per-page
                   :json-key "itemsPerPage")
   (resources :json-type (:list external-user)
              :initarg :resources
              :json-key "Resources"))
  (:metaclass ext-json-serializable-class))


(defclass error-response ()
  ((schemas :initform '("urn:ietf:params:scim:schemas:core:2.0:Error")
            :json-type (:list :string)
            :json-key "schemas")
   (scim-type :json-type (or null :string)
              :json-key "scimType"
              :initarg :type)
   (detail :json-type :string
           :json-key "detail"
           :initform "NA"
           :initarg :detail)
   (status :json-type :string
           :json-key "status"
           :initarg :status))
  (:metaclass ext-json-serializable-class))

(defmethod initialize-instance :after ((self error-response) &key type detail status)
  (declare (ignore detail status))
  (unless type
    (slot-makunbound self 'scim-type)))
