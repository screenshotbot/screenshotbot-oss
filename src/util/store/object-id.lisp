;; Copyright 2018-Present Modern Interpreters Inc.
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage #:util/store/object-id
  (:nicknames #:util/object-id)
  (:use #:cl
        #:bknr.datastore)
  (:import-from #:bknr.indices
                #:unique-index)
  (:import-from #:util/store
                #:location-for-oid
                #:defindex)
  (:export #:object-with-oid
	   #:object-with-unindexed-oid
	   #:find-by-oid
	   #:oid
       #:oid-array
       #:creation-time-from-oid
       #:oid-struct-or-array))
(in-package #:util/object-id)

(defstruct oid
  "An object to efficiently represent a Mongo Object-ID. As of 11/27/22,
 this isn't being used but will be soon."
  (arr))

(defmethod fset:compare ((a oid) (b oid))
  (fset:compare-slots
   a b
   #'oid-arr))

(defmethod print-object ((self oid) stream)
  (cond
    ((or
      *print-readably*
      *print-escape*)
     (format stream "#<OID ~a>" (fast-oid-str (oid-arr self))))
    (t
     (format stream (fast-oid-str (oid-arr self))))))

(defmethod bknr.datastore::encode-object ((self oid) stream)
  ;; M for MongoId, O is being used!
  (bknr.datastore::%write-tag #\M stream)
  (write-sequence (oid-arr self) stream))

(defmethod bknr.datastore::decode-object ((tag (eql #\M)) stream)
  (let ((arr (make-array 12 :element-type '(unsigned-byte 8))))
    (read-sequence arr stream)
    (make-oid :arr arr)))

;; TODO: rename to new-oid
(defun %make-oid ()
  (make-oid
   :arr
   (mongoid:oid)))

(defindex +oid-index+ 'unique-index
  :test 'equalp
  :slot-name 'oid)

;;;; reloading this object is bad. I thought I had fixed this, but
;;;; it's still buggy inside of my patched version of bknr.datastore
(defclass object-with-oid (store-object)
  ((oid
    :initarg :oid
    :accessor oid-struct-or-array
    :index +oid-index+
    :index-reader %find-by-oid))
  (:metaclass persistent-class)
  (:default-initargs :oid (%make-oid)))

(defun migrate-oids ()
  (loop for obj in (class-instances 'object-with-oid)
        do (let ((oid (oid-struct-or-array obj)))
             (unless (or (stringp oid) (oid-p oid))
               (assert (vectorp oid))
               (with-transaction ()
                (setf (oid-struct-or-array obj) (make-oid :arr (oid-array obj))))))))

(defmethod oid-array (self)
  (let ((ret (oid-struct-or-array self)))
    (cond
      ((oid-p ret) (oid-arr ret))
      ((stringp ret) (mongoid:oid ret))
      (t ret))))

(defclass object-with-unindexed-oid (store-object)
  ((oid
    :initform (%make-oid)
    :accessor oid-bytes))
  (:metaclass persistent-class))

(defun find-by-oid (oid &optional type)
  "oid can be an array, a string, or an object of type OID"
  (let* ((arr (if (oid-p oid)
                  (oid-arr oid)
                  (mongoid:oid oid)))
         (obj (or
               (%find-by-oid
                (make-oid :arr arr))
               ;; For backward compatibility
               (%find-by-oid arr))))
    (when type
      (unless (typep obj type)
        (error "Object ~s isn't of type ~s" obj type)))
    obj))

(#+lispworks defconstant
 #-lispworks defparameter +e+ "0123456789abcdef")

(defun fast-oid-str (oid)
  (declare (optimize (speed 3)
                     (debug 0)
                     (safety 0))
           (type (array (unsigned-byte 8))))
  (let ((hex-string (make-string 24)))
    (loop for i fixnum from 0 below 12
          do (let ((out (* 2 i)))
               (multiple-value-bind (top left) (floor (aref oid i) 16)
                 (setf (aref hex-string out)
                       (aref +e+ top))
                 (setf (aref hex-string (1+ out))
                       (aref +e+ left)))))
    hex-string))

;; COPYPASTA from scheduled-jobs
;;;;;;;;;;;;;;;;;;;;;;
;; https://lisptips.com/post/11649360174/the-common-lisp-and-unix-epochs
(defvar *unix-epoch-difference*
  (encode-universal-time 0 0 0 1 1 1970 0))

(defun universal-to-unix-time (universal-time)
  (- universal-time *unix-epoch-difference*))

(defun unix-to-universal-time (unix-time)
  (+ unix-time *unix-epoch-difference*))

(defun get-unix-time ()
  (universal-to-unix-time (get-universal-time)))
;;;;;;;;;;;;;;;;;;;;;;;

(defmethod creation-time-from-oid ((object object-with-oid))
  (let* ((oid-arr (oid-array object))
         (unix (cl-mongo-id:get-timestamp oid-arr)))
    (unix-to-universal-time unix)))

(defmethod is-recent-p ((object object-with-oid) &key (days 14))
  (> (creation-time-from-oid object)
     (- (get-universal-time)
        (* days 24 3600))))

(defgeneric oid (obj &key stringp))

(defmethod oid (obj &key (stringp t))
  "Returns the oid of the object. If :STRINGP is T, then we convert the
oid to a string. Otherwise we return the OID object as is."
  (cond
    (stringp
     (fast-oid-str (oid-array obj)))
    (t
     (oid-struct-or-array obj))))

(defmethod location-for-oid ((root pathname) (oid oid) &key suffix)
  (location-for-oid
   root
   (oid-arr oid)
   :suffix suffix))
