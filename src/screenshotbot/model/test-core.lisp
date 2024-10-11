;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/model/test-core
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/model/core
                #:ensure-slot-boundp
                #:generate-api-key)
  (:import-from #:bknr.datastore
                #:class-instances
                #:persistent-class
                #:store-object)
  (:import-from #:util/store/store
                #:with-test-store)
  (:import-from #:fiveam-matchers/core
                #:does-not
                #:has-all
                #:assert-that)
  (:import-from #:fiveam-matchers/lists
                #:contains)
  (:local-nicknames (#:a #:alexandria)))
(in-package :screenshotbot/model/test-core)

(util/fiveam:def-suite)

(defclass foo (store-object)
  ((slot1 :initarg :slot1))
  (:metaclass persistent-class))

(def-fixture state ()
  (with-test-store ()
    (&body)))

(test ensure-slot-boundp
  (with-fixture state ()
    (loop for i from 0 to 2000
          do (make-instance 'foo))
    (is (not (slot-boundp (car (class-instances 'foo)) 'slot1)))
    (ensure-slot-boundp 'foo 'slot1)
    (is (slot-boundp (car (class-instances 'foo)) 'slot1))
    (assert-that
     (loop for obj in (class-instances 'foo)
           collect
           (slot-boundp obj 'slot1))
     (does-not
      (contains nil)))))
