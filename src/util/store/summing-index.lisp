;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/store/summing-index
  (:use #:cl)
  (:import-from #:bknr.indices
                #:index-remove
                #:index-get
                #:index-clear
                #:index-add
                #:index-reinitialize)
  (:import-from #:alexandria
                #:when-let))
(in-package :util/store/summing-index)

(defclass summing-index ()
  ((key :initarg :key
             :reader key-fn)
   (value :initarg :value
          :reader value-fn)
   (map :initform (make-hash-table :test #'eql)
        :reader %map))
  (:documentation "An index that sums a particular field by a particular key"))

(defmethod index-reinitialize ((new-index summing-index)
                               (old-index summing-index))
  (loop for key being the hash-keys of (%map old-index)
          using (hash-value v)
        do (setf (gethash key (%map new-index)) v)))


(defmethod index-add ((self summing-index)
                      obj)
  (when-let ((key (funcall (key-fn self) obj))
             (value (funcall (value-fn self) obj)))
    (incf (gethash key (%map self) 0)
          value)))

(defmethod index-clear ((self summing-index))
  (clrhash (%map self)))

(defmethod index-get ((self summing-index)
                      key)
  (gethash key (%map self) 0))

(defmethod index-remove ((self summing-index)
                         obj)
  (when-let ((key (funcall (key-fn self) obj))
             (value (funcall (value-fn self) obj)))
    (decf (gethash key (%map self) 0)
          value)))


