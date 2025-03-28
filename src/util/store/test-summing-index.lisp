;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/store/test-summing-index
  (:use #:cl
        #:fiveam)
  (:import-from #:bknr.datastore
                #:delete-object
                #:store-object
                #:persistent-class)
  (:import-from #:util/store/summing-index
                #:summing-index)
  (:import-from #:util/store/store
                #:with-test-store)
  (:import-from #:bknr.indices
                #:index-reinitialize
                #:index-clear
                #:index-get))
(in-package :util/store/test-summing-index)


(util/fiveam:def-suite)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *summing-index*
    (make-instance 'summing-index
                   ;; Note that #' syntax doesn't quite work here
                   ;; unless we also move the defgeneric to
                   ;; eval-when. Not worth it.
                   :key '%company
                   :value '%size)))

(defclass fake-image (store-object)
  ((company :initarg :company
            :initform nil
            :accessor %company)
   (size :initarg :size
         :initform nil
         :accessor %size))
  (:metaclass persistent-class)
  (:class-indices
   (summing-index
    :index *summing-index*
    :slots (company size))))

(def-fixture state ()
  (with-test-store ()
    (&body)))

(test simple-creation
  (with-fixture state ()
    (finishes
     (make-instance 'fake-image
                    :company :company-1
                    :size 20))
    (is (eql 20 (index-get *summing-index* :company-1)))
    (make-instance 'fake-image
                   :company :company-1
                   :size 10)
    (is (eql 30 (index-get *summing-index* :company-1)))
    (index-clear *summing-index*)
    (is (eql 0 (index-get *summing-index* :company-1)))))

(test index-reinialize
  (with-fixture state ()
    (make-instance 'fake-image
                   :company :company-1
                   :size 20)
    (let ((other-index (make-instance 'summing-index
                                      :key #'%company
                                      :value #'%size)))
      (index-reinitialize other-index *summing-index*))
    (is (eql 20 (index-get *summing-index* :company-1)))))

(test when-key-is-NIL
  (with-fixture state ()
    (make-instance 'fake-image
                   :company :company-1
                   :size 20)
    (finishes
      (make-instance 'fake-image
                     :company :company-1))
    (finishes
      (make-instance 'fake-image
                     :size 30))))

(test deleting-objects
  (with-fixture state ()
    (let ((im1 (make-instance 'fake-image
                              :company :company-1
                              :size 20))
          (im2 (make-instance 'fake-image
                              :company :company-1
                              :size 10)))
      (is (eql 30 (index-get *summing-index* :company-1)))
      (delete-object im2)
      (is (eql 20 (index-get *summing-index* :company-1)))
      (delete-object im1))))

(test can-delete-slots-with-NIL
  (with-fixture state ()
    (make-instance 'fake-image
                   :company :company-1
                   :size 20)
    (finishes
      (delete-object (make-instance 'fake-image
                                    :company :company-1)))
    (finishes
      (delete-object (make-instance 'fake-image
                                    :size 30)))))



(test setting-key
  (with-fixture state ()
    (finishes
     (make-instance 'fake-image
                    :company :company-1
                    :size 20))
    (is (eql 20 (index-get *summing-index* :company-1)))
    (let ((im2 (make-instance 'fake-image
                              :company :company-1
                              :size 10)))
      (is (eql 30 (index-get *summing-index* :company-1)))
      (setf (%company im2) :company-2)
      (is (eql 20 (index-get *summing-index* :company-1))))))


(test setting-value
  (with-fixture state ()
    (finishes
     (make-instance 'fake-image
                    :company :company-1
                    :size 20))
    (is (eql 20 (index-get *summing-index* :company-1)))
    (let ((im2 (make-instance 'fake-image
                   :company :company-1
                   :size 10)))
      (is (eql 30 (index-get *summing-index* :company-1)))
      (setf (%size im2) 5)
      (is (eql 25 (index-get *summing-index* :company-1))))))
