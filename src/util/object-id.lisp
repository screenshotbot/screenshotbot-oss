;; Copyright 2018-Present Modern Interpreters Inc.
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage #:util/object-id
  (:use #:cl
	#:bknr.datastore)
  (:export #:object-with-oid
	   #:object-with-unindexed-oid
	   #:find-by-oid
	   #:oid))
(in-package #:util/object-id)

(defun %make-oid ()
  (mongoid:oid))

;;;; reloading this object is bad. I thought I had fixed this, but
;;;; it's still buggy inside of my patched version of bknr.datastore
(defclass object-with-oid (store-object)
  ((oid
    :initarg :oid
    :index-type bknr.indices:unique-index
    :index-initargs (:test 'equalp)
    :index-reader %find-by-oid))
  (:metaclass persistent-class))

(defmethod initialize-instance :around ((obj object-with-oid)
                                        &rest args
                                        &key oid
                                        &allow-other-keys)
  (cond
    (oid
     (call-next-method))
    (t
     (apply #'call-next-method obj :oid (%make-oid) args))))

(defclass object-with-unindexed-oid (store-object)
  ((oid
    :initform (%make-oid)
    :accessor oid-bytes))
  (:metaclass persistent-class))

(defun find-by-oid (oid &optional type)
  (let* ((oid (mongoid:oid oid))
         (obj (%find-by-oid oid)))
    (when type
      (unless (typep obj type)
        (error "Object ~s isn't of type ~s" obj type)))
    obj))

(defconstant +e+ "0123456789abcdef")

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


(defmethod oid ((obj object-with-oid))
  (with-slots (oid) obj
    (fast-oid-str oid)))

(defmethod oid ((obj object-with-unindexed-oid))
  (with-slots (oid) obj
    (Fast-oid-str oid)))
