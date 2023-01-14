;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :gatekeeper/test-gatekeeper
  (:use #:cl
        #:fiveam)
  (:import-from #:util/store
                #:with-test-store)
  (:import-from #:gatekeeper/gatekeeper
                #:access-control
                #:access-controls
                #:gatekeeper)
  (:import-from #:bknr.datastore
                #:with-transaction))
(in-package :gatekeeper/test-gatekeeper)

(util/fiveam:def-suite)

(def-fixture state (&key name default)
  (with-test-store ()
    (let ((gk (make-instance 'gatekeeper
                     :name name
                     :default default)))
     (let ((obj :dummy-obj))
       (&body)))))

(test simple-gk-check
  (with-fixture state ()
    (is (eql t (gk:check :non-exist obj :default t)))
    (is (eql nil (gk:check :non-exist obj :default nil)))))

(test gk-if-gk-exists
  (with-fixture state (:name "BLEH" :default t)
    (is (eql t (gk:check :bleh obj :default nil)))
    (is (eql t (gk:check :bleh obj :default t)))
    (with-transaction ()
     (setf (access-controls gk)
           (list
            (make-instance 'access-control
                           :type :deny
                           :objects (list obj)))))
    (is (eql nil (gk:check :bleh obj :default nil)))))

(test gk-if-gk-exists-but-denys-by-default
  (with-fixture state (:name "BLEH" :default nil)
    (is (eql nil (gk:check :bleh obj :default t)))
    (is (eql nil (gk:check :bleh obj :default nil)))
    (with-transaction ()
     (setf (access-controls gk)
           (list
            (make-instance 'access-control
                           :type :allow
                           :objects (list obj)))))
    (is (eql t (gk:check :bleh obj :default nil)))))
