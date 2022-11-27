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
                #:make-oid
                #:%make-oid
		#:object-with-oid
                #:oid
                #:find-by-oid)
  (:import-from #:util/store
                #:with-test-store)
  (:import-from #:bknr.indices
                #:hash-index)
  (:import-from #:bknr.datastore
                #:persistent-class)
  (:import-from #:bknr.datastore
                #:encode-object)
  (:import-from #:bknr.datastore
                #:decode-object)
  (:import-from #:bknr.datastore
                #:encode)
  (:import-from #:bknr.datastore
                #:decode))
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

(test encode-decode-oid
  (uiop:with-temporary-file (:stream s :element-type '(unsigned-byte 8)
                             :direction :io)
    (let ((oid-arr (mongoid:oid)))
      (let ((oid (make-oid :arr oid-arr))
            (oid2 (make-oid :arr oid-arr)))
        (is (equalp oid oid2))
        (encode oid s)
        (is (= 13 (file-position s)))
        (file-position s 0)
        (let ((oid-read (decode s)))
          (is (equalp oid oid-read)))))))
