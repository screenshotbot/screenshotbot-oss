;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/model/finalized-commit
  (:use #:cl)
  (:import-from #:bknr.datastore
                #:persistent-class
                #:store-object)
  (:import-from #:util/store/store
                #:defindex
                #:with-class-validation)
  (:import-from #:util/store/fset-index
                #:fset-set-index))
(in-package :screenshotbot/model/finalized-commit)

(defindex +commit-index+
  'fset-set-index
  :slot-name '%commit)

(with-class-validation
 (defclass finalized-commit (store-object)
   ((%company :initarg :company
              :reader company)
    (%commit :initarg :commit
             :index +commit-index+
             :index-reader %finalized-commits-for-commit))
   (:metaclass persistent-class)))

(defun commit-finalized-p (company commit)
  (fset:do-set (fc (%finalized-commits-for-commit commit))
    (when  (eql company (company fc))
      (return t))))
