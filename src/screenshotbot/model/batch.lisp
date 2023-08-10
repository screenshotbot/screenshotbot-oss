;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/model/batch
  (:use #:cl)
  (:import-from #:bknr.datastore
                #:persistent-class
                #:store-object)
  (:import-from #:util/store/store
                #:with-class-validation))
(in-package :screenshotbot/model/batch)

(with-class-validation
  (defclass batch (store-object)
    ()
    (:metaclass persistent-class)))
