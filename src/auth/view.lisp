;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :auth/view
  (:use #:cl
        #:auth))
(in-package :auth/view)

(define-condition no-access-error (error)
  ((user :initarg :user
         :accessor error-user)
   (obj :initarg :obj
        :accessor error-obj)))

(defmethod print-object ((e no-access-error) out)
  (format out "User ~S can't access ~S" (error-user e) (error-obj e)))

(defgeneric can-view (obj user)
  (:documentation "Can the USER view object OBJ?"))

(defgeneric can-public-view (obj)
  (:documentation "Can the public (non-logged-in user?) view the object OBJ?")
  (:method (obj)
    nil))


(defun can-view! (&rest objects)
  (let ((user (auth:current-user)))
    (dolist (obj objects)
      (unless (or
               (can-public-view obj)
               (and user
                    (can-view obj user)))
        (restart-case
            (error 'no-access-error
                    :user user
                    :obj obj)
          (give-access-anyway ()
            nil))))))

(defgeneric can-edit (obj user)
  (:method (obj user)
    nil)
  (:documentation "Can the USER edit object OBJ?"))

(defmethod can-edit! (&rest objects)
  (let ((user (current-user)))
    (dolist (obj objects)
      (unless (can-edit obj user)
        (error 'no-access-error :user user :obj obj)))))

(defmethod can-edit :around (obj user)
  "In order to edit something, we also need to be able to view it."
  (and
   user
   (can-view obj user)
   (call-next-method)))
