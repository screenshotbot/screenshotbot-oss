;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/model/test-constant-string
  (:use #:cl
        #:fiveam)
  (:import-from #:util/store/store
                #:with-test-store)
  (:import-from #:screenshotbot/model/constant-string
                #:ensure-slot-constant-string
                #:constant-string-string
                #:constant-string)
  (:import-from #:bknr.datastore
                #:persistent-class
                #:store-object))
(in-package :screenshotbot/model/test-constant-string)


(util/fiveam:def-suite)

(def-fixture state ()
  (with-test-store ()
    (&body)))

(test simple-constant-string
  (with-fixture state ()
    (is (eql nil (constant-string nil)))
    (is (typep (constant-string "foobar") 'constant-string))
    (is (eql
         (constant-string "foobar")
         (constant-string "foobar")))
    (is (not
         (eql
          (constant-string "foobar")
          (constant-string "carbar"))))))

(test coerce-back-into-string
  (with-fixture state ()
    (is (equal "foobar" (constant-string-string (constant-string "foobar"))))))

(test print-object
  (with-fixture state ()
    (is (equal "foobar" (format nil "~a" (constant-string "foobar"))))))

(test comparison
  (with-fixture state ()
    (is (eql
         :less
         (fset:compare "abc"
                       "bar")))
    (is (eql
         :less
         (fset:compare (constant-string "abc")
                       "bar")))
    (is (eql
         :less
         (fset:compare (constant-string "abc")
                       (constant-string "bar"))))
    (is (eql
         :greater
         (fset:compare (constant-string "bar")
                       (constant-string "abc"))))
    (is (fset:equal?
         (constant-string "car")
         "car"))))

(defclass foo (store-object)
  ((bar :initarg :bar))
  (:metaclass persistent-class))

(test ensure-slot-constant-string
  (with-fixture state ()
   (let ((obj (make-instance 'foo
                             :bar "car")))
     (ensure-slot-constant-string
      (list obj)
      'bar)
     (is (typep (slot-value obj 'bar)
                'constant-string)))))
