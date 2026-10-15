;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/scim/dto
  (:use #:cl)
  (:import-from #:util/json-mop
                #:ext-json-serializable-class)
  (:import-from #:screenshotbot/scim/model
                #:user-to-dto
                #:scim-user-user
                #:scim-user-user-name
                #:scim-user-activep
                #:scim-user-external-id
                #:scim-user-emails
                #:scim-user-v2)
  (:import-from #:util/store/object-id
                #:oid)
  (:export
   #:list-response-resources
   #:external-user-id))
(in-package :screenshotbot/scim/dto)

(defclass external-name ()
  ((formatted :json-type :string
              :json-key "formatted"
              :initarg :formatted
              :accessor name-formatted)
   (given :json-type :string
          :json-key "givenName"
          :initarg :given
          :accessor name-given)
   (family-name :json-type :string
                :json-key "familyName"
                :initarg :family-name
                :accessor name-family-name))
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
         :accessor external-user-name)
   (display-name :json-type :string
                 :initarg :display-name
                 :json-key "displayName"
                 :accessor external-user-display-name)
   (id :json-type :string
       :initarg :id
       :json-key "id"
       :reader external-user-id)
   (external-id :json-type :string
                :initarg :external-id
                :json-key "externalId"
                :reader external-user-external-id)
   (activep :json-type :bool
            :json-key "active"
            :initarg :activep
            :accessor external-user-activep)
   (emails :json-type (:list external-email)
           :initform nil
           :initarg :emails
           :json-key "emails"
           :reader external-user-emails))
  (:metaclass ext-json-serializable-class))

(defun ensure-external-user-name (dto)
  (unless (slot-boundp dto 'name)
    (setf (external-user-name dto) (make-instance 'external-name)))
  (external-user-name dto))

(defun dto-full-name (dto)
  "The user's full name from the SCIM DTO, or NIL if there isn't one.
Prefers displayName, then name.formatted, then givenName + familyName."
  (flet ((value (obj slot)
           (when (and obj (slot-boundp obj slot))
             (slot-value obj slot))))
    (let ((name (value dto 'name)))
      (find-if #'str:non-empty-string-p
               (list
                (value dto 'display-name)
                (value name 'formatted)
                (str:join " "
                          (remove-if-not #'str:non-empty-string-p
                                         (list (value name 'given)
                                               (value name 'family-name)))))))))

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
              :reader list-response-resources
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

(defmethod user-to-dto ((user scim-user-v2))
  (apply
   #'make-instance 'external-user
   :id (oid user)
   :external-id (ignore-errors
                 (scim-user-external-id user))
   :activep (scim-user-activep user)
   :user-name (scim-user-user-name user)
   :emails
   (loop for email in (scim-user-emails user)
         for count from 0
         collect
         (make-instance 'external-email
                        :type "work"
                        :value email
                        :primary (eql 0 count)))
   ;; Only name.formatted, not displayName: a PATCH of displayName
   ;; should take precedence over what we return here.
   (let ((full-name (auth:user-full-name (scim-user-user user))))
     (when (str:non-empty-string-p full-name)
       (list :name (make-instance 'external-name
                                  :formatted full-name))))))
