;; Copyright 2018-Present Modern Interpreters Inc.
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util.model.test-object-id
  (:use #:cl
	#:alexandria
        #:fiveam)
  (:import-from #:util/object-id
		#:object-with-oid
                #:oid
                #:find-by-oid)
  (:import-from #:util/store
                #:with-test-store)
  (:import-from #:bknr.indices
                #:hash-index)
  (:import-from #:bknr.datastore
                #:persistent-class))
(in-package :util.model.test-object-id)

(def-suite* :util.model.test-object-id)

#-buck
(test simple-creation-and-finding
  (with-test-store ()
   (let ((obj (make-instance 'object-with-oid)))
     (is (eql obj
              (find-by-oid (oid obj)))))))

(test indices-dont-fail
  (with-test-store ()
    (let ((name (intern "dummy-name")))
      (unwind-protect
           (let* ((expr `(defclass ,name (object-with-oid)
                           ((test-key :initarg :key
                                      :index-type hash-index
                                      :index-values all-test-classes))
                           (:metaclass persistent-class))))

             (eval expr)
             (let ((obj (make-instance name :key 2)))
               (is (equal (list obj)
                          (all-test-classes)))
               (eval expr)
               (is (equal (list obj)
                          (all-test-classes))))))
      (unintern name))))
