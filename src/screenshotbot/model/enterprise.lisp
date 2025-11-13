;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/model/enterprise
  (:use #:cl)
  (:import-from #:bknr.datastore
                #:persistent-class
                #:store-object)
  (:import-from #:util/store/store
                #:defindex)
  (:import-from #:util/store/fset-index
                #:fset-unique-index)
  (:export #:enterprise-install
           #:enterprise-install-domain))
(in-package :screenshotbot/model/enterprise)

(defindex +domain-index+
  'fset-unique-index
  :slot-name 'domain)

(defclass enterprise-install (store-object)
  ((domain :initarg :domain
           :index +domain-index+
           :accessor enterprise-install-domain))
  (:metaclass persistent-class))

