;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/android/view
  (:use #:cl)
  (:import-from #:screenshotbot/android/api
                #:context-delegate)
  (:export
   #:def-view))
(in-package :screenshotbot/android/view)

(defclass view ()
  ((delegate :initarg :delegate
             :accessor delegate)
   (context :initarg :context)
   (constructor :initarg :constructor
                :reader java-constructor)))

(defmacro def-view (name parents java-class-name
                    slots)
  (let ((constructor (intern (format nil "%MAKE-~a" name))))
    `(progn
       (lw-ji:define-java-constructor ,constructor
         ,java-class-name)
       (defclass ,name ,parents
         ,slots
         (:default-initargs :constructor ',constructor)))))

(defmethod initialize-instance :after ((self view) &key context)
  (setf (delegate self)
        (funcall (java-constructor self) (context-delegate context))))

