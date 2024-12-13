;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/model/sso
  (:use #:cl)
  (:import-from #:bknr.datastore
                #:persistent-class
                #:store-object)
  (:import-from #:util/store/store
                #:defindex
                #:with-class-validation)
  (:import-from #:util/store/fset-index
                #:fset-unique-index))
(in-package :screenshotbot/model/sso)

(defindex +company-index+
  'fset-unique-index
  :slot-name '%company)

(with-class-validation
  (defclass sso-provider (store-object)
    ((%company :initarg :company
               :index +company-index+
               :index-reader sso-provider-for-company 
               :reader sso-company))
    (:metaclass persistent-class)))


