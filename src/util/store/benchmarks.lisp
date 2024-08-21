;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/store/benchmarks
  (:use #:cl)
  (:import-from #:util/benchmark
                #:def-benchmark)
  (:import-from #:util/store/store
                #:with-test-store)
  (:import-from #:bknr.datastore
                #:store-object
                #:persistent-class))
(in-package :util/store/benchmarks)

(defclass foo (store-object)
  ((a :initarg :a) b c d e f (g :initarg :g) h i j k l m (n :initarg :n))
  (:metaclass persistent-class))

(defclass standard-foo ()
  (a b c d e f (g :initarg :g :reader g) h i j k l m n))

(def-benchmark object-destroyed-p-benchmark ()
  (with-test-store ()
    (let ((obj (make-instance 'foo)))
      (benchmark:measure
        (bknr.datastore::object-destroyed-p obj)))))

(def-benchmark slot-value-benchmark ()
  (with-test-store ()
    (let ((obj (make-instance 'foo :g 42)))
      (benchmark:measure
        (slot-value obj 'g)))))

(def-benchmark slot-value-early ()
  (with-test-store ()
    (let ((obj (make-instance 'foo :a 42)))
      (benchmark:measure
        (slot-value obj 'a)))))

(def-benchmark standard-slot-access ()
  (let ((obj (make-instance 'standard-foo :g 42)))
    (let ((num 0))
     (benchmark:measure
       (incf num (g obj))))))
