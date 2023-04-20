;; Copyright 2018-Present Modern Interpreters Inc.
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/store/test-object-id
  (:use #:cl
	#:alexandria
        #:fiveam)
  (:import-from #:util/object-id
                #:fast-oid-str
                #:oid-arr
                #:migrate-oids
                #:oid-p
                #:oid-struct-or-array
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
                #:decode)
  (:import-from #:fiveam-matchers/lists
                #:contains)
  (:import-from #:bknr.datastore
                #:class-instances))
(in-package :util/store/test-object-id)

(util/fiveam:def-suite)

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

(test print-object-for-oid
  (let ((oid (make-oid
              :arr (make-array 12
                          :initial-contents '(1 1 1 1
                                              1 1 1 1
                                              1 1 1 1)))))
    (is (equal "/foo/010101010101010101010101"
               (format nil "/foo/~a" oid)))
    (is (equal "/foo/010101010101010101010101"
               (let ((*print-escape* t))
                 ;; In prod, for whatever reason *print-escape* is on
                 ;; globally. We still want to make sure
                 ;; it's rendered as a string here.
                 (format nil "/foo/~a" oid))))
    (is (equal "/foo/#<OID 010101010101010101010101>"
               (format nil "/foo/~S" oid)))))

(test migrate-objects
  (with-test-store ()
    (let* ((oid-1 (mongoid:oid))
           (oid-2 (%make-oid))
           (oid-3 (fast-oid-str (mongoid:oid)))
           (obj1 (make-instance 'object-with-oid
                                :oid oid-1))
           (obj2 (make-instance 'object-with-oid
                                :oid oid-2))
           (obj3 (make-instance 'object-with-oid
                                :oid oid-3)))
      (is (oid-p (oid-struct-or-array obj2)))
      (is (not (oid-p (oid-struct-or-array obj1))))
      (is (not (oid-p (oid-struct-or-array obj1))))
      (migrate-oids)
      (is (oid-p (oid-struct-or-array obj2)))
      (is (not (oid-p (oid-arr (oid-struct-or-array obj2)))))
      (is (oid-p (oid-struct-or-array obj1)))
      (is (not (oid-p (oid-arr (oid-struct-or-array obj1)))))
      (is (oid-p (oid-struct-or-array obj1))))))


(test object-ids-are-maintained
  (let ((oid))
   (tmpdir:with-tmpdir (dir)
     (with-test-store (:dir dir)
       (setf oid (oid (make-instance 'object-with-oid))))
     (with-test-store (:dir dir)
       (is (equalp oid
                   (oid (car
                         (class-instances 'object-with-oid)))))))))
