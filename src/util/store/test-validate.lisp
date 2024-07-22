;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/store/test-validate
  (:use :cl)
  (:import-from #:bknr.indices
                #:index-remove
                #:index-values)
  (:import-from #:fiveam-matchers/core
                #:assert-that)
  (:import-from #:fiveam-matchers/lists
                #:contains)
  (:import-from #:it.bese.fiveam
                #:def-fixture
                #:finishes
                #:is-false
                #:signals
                #:test
                #:with-fixture)
  (:import-from #:util/store/fset-index
                #:fset-set-index
                #:corrupted-index)
  (:import-from #:util/store/store
                #:defindex
                #:with-test-store)
  (:import-from #:util/store/validate
                #:validate-indices)
  (:import-from #:bknr.datastore
                #:store-object
                #:persistent-class))
(in-package :util/store/test-validate)


(util/fiveam:def-suite)


(defclass my-object (store-object)
  ((key :initarg :key
        :index-type fset-set-index
        :index-var +my-object-index+
        :accessor key))
  (:metaclass persistent-class))

(def-fixture validate-indices ()
  (with-test-store ()
    (&body)))

(test validate-indices-happy-path
  (with-fixture validate-indices ()
    (finishes (validate-indices))
    (let ((obj (make-instance 'my-object :key "foo")))
      (finishes (validate-indices))
      (index-remove +my-object-index+ obj)
      (let ((auto-restart:*global-enable-auto-retries-p* nil))
        (signals corrupted-index
          (validate-indices)))

      (is-false (index-values +my-object-index+))
      (validate-indices :fix-by-default t)
      (assert-that (index-values +my-object-index+)
                   (contains obj)))))
