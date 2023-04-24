;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/store/fset-index
  (:use #:cl)
  (:import-from #:bknr.indices
                #:index-reinitialize)
  (:import-from #:bknr.indices
                #:index-add)
  (:import-from #:bknr.indices
                #:index-get)
  (:import-from #:bknr.indices
                #:index-clear)
  (:import-from #:bknr.indices
                #:index-remove))
(in-package :util/store/fset-index)

(defclass abstract-fset-index ()
  ((slot-name :initarg :slots
              :reader %slots)
   (map :initform (fset:empty-map)
        :accessor %map)))

#-lispworks
(defvar *lock* (bt:make-lock))

(defmacro update-map (self expr)
  #-lispworks
  `(bt:with-lock-held (*lock*)
     (setf (%map ,self) ,expr))
  #+lispworks
  `(system:atomic-exchange
    (slot-value ,self 'map)
    ,expr))

(defun %slot-name (self)
  (car (%slots self)))

(defmethod index-reinitialize ((new-index abstract-fset-index)
                               (old-index abstract-fset-index))
  (update-map new-index
              (%map old-index))
  new-index)


(defclass fset-unique-index (abstract-fset-index)
  ())

(defmethod index-add ((self fset-unique-index)
                      obj)
  (update-map self
              (fset:with (%map self)
                         (slot-value obj (%slot-name self))
                         obj)))

(defmethod index-get ((self abstract-fset-index) key)
  (fset:lookup (%map self) key))

(defmethod index-clear ((self abstract-fset-index))
  (update-map self (fset:empty-map)))

(defmethod index-remove ((self fset-unique-index)
                         obj)
  (update-map
   self
   (fset:less (%map self)
              (slot-value obj (%slot-name self)))))




