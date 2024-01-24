;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/store/encodable
  (:use #:cl)
  (:import-from #:bknr.datastore
                #:find-slot-name-with-automatic-rename
                #:%read-tag
                #:decode
                #:encode
                #:%write-tag
                #:decode-object
                #:encode-object))
(in-package :util/store/encodable)

(defclass encodable ()
  ())

(defmethod encode-object ((self encodable) stream)
  (%write-tag #\E stream)
  (encode (type-of self) stream)
  (%write-tag #\0 stream)
  (encode
   (loop for slot in (closer-mop:class-slots (class-of self))
         for slot-name = (closer-mop:slot-definition-name slot)
         if (slot-boundp self slot-name)
         collect (cons
                  slot-name
                  (slot-value self slot-name)))
   stream))

(defmethod decode-object ((tag (eql #\E)) stream)
  (let* ((type (decode stream))
         (version (%read-tag stream))
         (class (find-class type)))
    (let* ((obj (allocate-instance class))
           (slot-pairs (decode stream)))
      (loop for (slot-name . value) in slot-pairs
            do (setf (slot-value
                      obj
                      (find-slot-name-with-automatic-rename
                       class
                       slot-name))
                     value))
      obj)))
