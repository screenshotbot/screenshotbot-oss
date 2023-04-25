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
                #:fset-set-index
                #:fset-unique-index)
  (:import-from #:util/store/store
                #:with-test-store)
  (:import-from #:bknr.indices
                #:index-clear)
  (:import-from #:bknr.indices
                #:clear-slot-indices)
  (:import-from #:bknr.indices
                #:destroy-object)
  (:import-from #:bknr.indices
                #:destroy-object)
  (:import-from #:bknr.indices
                #:index-existing-error))
(in-package :util/store/test-fset-index)

(util/fiveam:def-suite)

(defclass test-object ()
  ((arg :initarg :arg
        :accessor %arg
        :index-type fset-unique-index
        :index-reader search-by-arg
        :index-values first-index-values))
  (:metaclass indexed-class))

(defclass test-object-2 ()
  ((arg :initarg :arg
        :index-type fset-set-index
        :index-reader second-by-arg
        :index-values second-index-values))
  (:metaclass indexed-class))

(def-fixture state ()
  (with-test-store ()
    (flet ((clear-indices ()
             (loop for class in '(test-object test-object-2)
                   do
                      (mapcar
                       #'clear-slot-indices
                       (closer-mop:class-slots (find-class class))))))
     (unwind-protect
          (&body)
       (clear-indices)))))

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


(defun make-set (&rest args)
  (fset:convert 'fset:set args))

(test simple-create-and-load-set
  (with-fixture state ()
    (let ((obj (make-instance 'test-object-2
                              :arg "foo")))
      (is (fset:equal? (make-set obj)
                       (second-by-arg "foo")))
      (is (fset:equal? (make-set)
                       (second-by-arg "car")))
      (let ((obj2 (make-instance 'test-object-2
                                 :arg "foo")))
        (is (fset:equal? (make-set obj obj2)
                         (second-by-arg "foo")))
        (destroy-object obj2)
        (is (fset:equal? (make-set obj)
                         (second-by-arg "foo")))
        (destroy-object obj)
        (is (fset:equal? (make-set)
                         (second-by-arg "foo")))))))

(test unbound-slot
  (with-fixture state ()
    (let ((obj (make-instance 'test-object)))
      (is (eql nil
               (search-by-arg "foo"))))
    (let ((obj (make-instance 'test-object-2)))
      (is (fset:equal? (fset:empty-set)
                       (second-by-arg "foo"))))))

(test update-slot
  (with-fixture state ()
    (let ((obj (make-instance 'test-object
                              :arg "foo")))
      (setf (%arg obj) "bar")
      (is (eql obj (search-by-arg "bar")))
      (is (eql nil (search-by-arg "foo")))
      (setf (%arg obj) "car")
      (is (eql obj (search-by-arg "car")))
      (is (eql nil (search-by-arg "foo"))))))


(test index-values
  (with-fixture state ()
    (let ((obj (make-instance 'test-object :arg "foo"))
          (obj-2 (make-instance 'test-object))
          (obj-3 (make-instance 'test-object :arg "bar")))
      (is (equal
           (list obj obj-3)
           (first-index-values))))))

(test index-values-for-set-mode
  (with-fixture state ()
    (let ((obj (make-instance 'test-object-2 :arg "foo"))
          (obj-2 (make-instance 'test-object-2))
          (obj-3 (make-instance 'test-object-2 :arg "bar")))
      (is (equal
           (list obj obj-3)
           (second-index-values))))))

(test nil-is-not-added-to-index
  (with-fixture state ()
    (let ((obj (make-instance 'test-object :arg nil)))
      (is (equal nil (first-index-values))))))

(test deleting-an-equivalent-object
  (with-fixture state ()
    (let ((obj (make-instance 'test-object :arg "foo")))
      (clear-indices) ;; make obj not part of an index
      (let ((obj-2 (make-instance 'test-object :arg "foo")))
        (destroy-object obj)
        (is (eql obj-2 (search-by-arg "foo")))))))

(test uniqueness
  (with-fixture state ()
    (let ((obj (make-instance 'test-object :arg "foo")))
      (signals index-existing-error
        (make-instance 'test-object :arg "foo"))
      ;; Make sure the index hasn't been modified
      (is (eql obj (search-by-arg "foo"))))))
