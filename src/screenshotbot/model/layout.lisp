;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/model/layout
  (:use #:cl)
  (:import-from #:util/store/fset-index
                #:fset-set-index)
  (:import-from #:bknr.datastore
                #:store-object
                #:persistent-class)
  (:import-from #:util/store/store
                #:with-class-validation))
(in-package :screenshotbot/model/layout)

(with-class-validation
 (defclass layout (store-object)
   ((%company :initarg :company
              :reader layout-company)
    (%hash :initarg :hash
           :reader layout-hash
           :index-type fset-set-index
           :index-reader %layouts-with-hash))
   (:metaclass persistent-class)))

(defun layout-with-hash (company hash)
  (fset:do-set (layout (%layouts-with-hash hash))
    (if (eql (layout-company layout) company)
        (return layout))))
