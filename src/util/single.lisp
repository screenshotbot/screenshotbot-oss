;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/single
  (:use #:cl)
  (:import-from #:bknr.datastore
                #:%encode-symbol)
  (:import-from #:bknr.datastore
                #:%decode-symbol)
  (:import-from #:bknr.datastore
                #:find-class-with-interactive-renaming)
  (:import-from #:bknr.datastore
                #:encode)
  (:import-from #:bknr.datastore
                #:encode)
  (:import-from #:bknr.datastore
                #:encode)
  (:import-from #:bknr.datastore
                #:decode)
  (:import-from #:bknr.datastore
                #:find-slot-name-with-interactive-rename)
  (:local-nicknames (#:a #:alexandria)))
(in-package :util/single)

(defvar *magick* (list
                  (char-code #\S)
                  (char-code #\G)))
(defparameter *version* 1)

(defmethod serialize (obj stream)
  (write-sequence *magick* stream)
  (write-byte *version* stream)
  (encode (type-of obj)
                  stream)
  (let ((slots (closer-mop:class-slots (class-of obj))))
    (dolist (slot slots)
      (let ((slot (closer-mop:slot-definition-name slot)))
       (when (slot-boundp obj slot)
         (encode slot stream)
         (encode (slot-value obj slot) stream)))))
  (encode nil stream))

(defmethod deserialize (stream)
  (let ((magick (list nil nil)))
    (read-sequence magick stream)
    (assert (equal magick *magick*))
    ;; version
    (let ((ver (read-byte stream)))
      (assert (eql *version* ver)))
    (let* ((class-name (decode stream)))
      (let ((class (find-class-with-interactive-renaming class-name)))
        (let ((instance (allocate-instance class)))
          (loop for slot = (decode stream)
                while slot
                do
                   (let ((slot (find-slot-name-with-interactive-rename class slot))
                         (value (decode stream)))
                     (setf (slot-value instance slot) value)))
          instance)))))
