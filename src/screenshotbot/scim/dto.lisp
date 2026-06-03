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

(defclass external-user ()
  ((schemas :initform '(("urn:ietf:params:scim:schemas:core:2.0:User"
                         "urn:ietf:params:scim:schemas:extension:enterprise:2.0:User"))
            :json-type (:list :string)
            :json-key "schemas")
   (user-name :json-type :string
              :json-key "userName"
              :reader external-user-user-name)
   (name :json-type external-name
         :json-key "name"
         :reader external-user-name))
  (:metaclass ext-json-serializable-class))


