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

(defclass external-user ()
  ((schemas :initform '(("urn:ietf:params:scim:schemas:core:2.0:User"
                         "urn:ietf:params:scim:schemas:extension:enterprise:2.0:User"))
            :json-type (:list :string)
            :json-key "schemas")
   (user-name :json-type :string
              :json-key "userName"
              :reader external-user-user-name))
  (:metaclass ext-json-serializable-class))


