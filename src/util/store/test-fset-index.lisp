;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/store/test-fset-index
  (:use #:cl
        #:fiveam)
  (:import-from #:bknr.indices
                #:indexed-class)
  (:import-from #:util/store/fset-index
                #:fset-unique-index)
  (:import-from #:util/store/store
                #:with-test-store)
  (:import-from #:bknr.indices
                #:index-clear)
  (:import-from #:bknr.indices
                #:clear-slot-indices)
  (:import-from #:bknr.indices
                #:destroy-object))
(in-package :util/store/test-fset-index)

(util/fiveam:def-suite)

(defclass test-object ()
  ((arg :initarg :arg
        :index-type fset-unique-index
        :index-reader search-by-arg))
  (:metaclass indexed-class))

(def-fixture state ()
  (with-test-store ()
    (unwind-protect
         (&body)
      (mapcar
       #'clear-slot-indices
       (closer-mop:class-slots (find-class 'test-object))))))

(test simple-create-and-load
  (with-fixture state ()
    (let ((obj (make-instance 'test-object
                              :arg "foo")))
      (is (eql obj
               (search-by-arg "foo")))
      (is (eql nil
               (search-by-arg "bar")))))
  (with-fixture state ()
    (is (eql nil
             (search-by-arg "foo")))))

(test destroy-object
  (with-fixture state ()
    (let ((obj (make-instance 'test-object
                              :arg "foo")))
      (is (eql obj
               (search-by-arg "foo")))
      (destroy-object obj)
      (is (eql nil (search-by-arg "foo"))))))
