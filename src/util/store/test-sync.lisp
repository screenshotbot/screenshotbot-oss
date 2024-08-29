;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/store/test-sync
  (:use #:cl
        #:fiveam)
  (:import-from #:util/store/store
                #:with-test-store)
  (:import-from #:bknr.datastore
                #:persistent-class
                #:store-object)
  (:import-from #:core/rpc/rpc
                #:call-rpc)
  (:import-from #:util/store/sync
                #:sync-sha-request)
  (:import-from #:fiveam-matchers/core
                #:assert-that)
  (:import-from #:fiveam-matchers/has-length
                #:has-length))
(in-package :util/store/test-sync)

(util/fiveam:def-suite)

(defclass foobar (store-object)
  ()
  (:metaclass persistent-class))

(test sync-happy-path
  (with-test-store ()
    (make-instance 'foobar)
    (let ((output (call-rpc (make-instance 'sync-sha-request))))
      (assert-that output
                   (has-length 64)))))
