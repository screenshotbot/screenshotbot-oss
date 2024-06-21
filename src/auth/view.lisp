;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :auth/view
  (:use #:cl
        #:auth)
  (:import-from #:auth/viewer-context
                #:anonymous-viewer-context
                #:logged-in-viewer-context
                #:viewer-context-user
                #:normal-viewer-context)
  (:import-from #:util/events
                #:push-event))
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

(defgeneric can-viewer-view (vc obj)
  (:argument-precedence-order obj vc))

(defmethod can-viewer-view ((vc normal-viewer-context) obj)
  (or
   (can-public-view obj)
   (call-next-method)))

(defmethod can-viewer-view (vc (obj null))
  (warn "can-viewer-view called on NIL object")
  nil)

(defmethod can-viewer-view ((vc anonymous-viewer-context) obj)
  (can-public-view obj))

(defmethod can-viewer-view ((vc logged-in-viewer-context) obj)
  (let ((user (viewer-context-user vc)))
    (and user
         (can-view obj user))))

(defun can-view! (&rest objects)
  (let ((vc (auth:viewer-context hunchentoot:*request*)))
    (dolist (obj objects)
      (unless (can-viewer-view vc obj)
        (restart-case
            (error 'no-access-error
                   :user (ignore-errors ;; may not have a user
                          (viewer-context-user vc))
                    :obj obj)
          (give-access-anyway ()
            nil))))))

(defmethod can-viewer-edit ((vc logged-in-viewer-context) obj)
  (let ((user (viewer-context-user vc)))
    (can-edit obj user)))

(defmethod can-view-with-normal-viewer-context (user obj)
  "This is a helper method just to make it easier to transition from
CAN-VIEW to CAN-VIEWER-VIEW."
  (warn "can-view-with-normal-viewer-context called with ~a, ~a" user obj)
  (can-viewer-view
   (make-instance 'normal-viewer-context
                  :user user)
   obj))

(defmethod can-edit-with-normal-viewer-context (user obj)
  "This is a helper method just to make it easier to transition from
CAN-EDIT to CAN-VIEWER-EDIT."
  (warn "can-edit-with-normal-viewer-context called with ~a, ~a" user obj)
  (can-viewer-edit
   (make-instance 'normal-viewer-context
                  :user user)
   obj))

(defmethod can-viewer-edit (vc obj)
  nil)

(defmethod can-viewer-edit :around (vc obj)
  (and
   (auth:can-viewer-view vc obj)
   (call-next-method)))

(defmethod can-viewer-view :around (vc obj)
  (let ((res (call-next-method)))
    (unless res
      ;; We are track failures so that a spike can indicate bugs in
      ;; the authorization logic, or a potential attacker.
      (push-event :can-viewer-view-failed
                  :object (format nil "~a" obj)
                  :vc (format nil "~a" vc)))
    res))

(defgeneric can-edit (obj user)
  (:method (obj user)
    nil)
  (:documentation "Can the USER edit object OBJ?"))

(defmethod can-edit! (&rest objects)
  (let ((vc (auth:viewer-context hunchentoot:*request*)))
   (dolist (obj objects)
     (unless (can-viewer-edit vc obj)
       (error 'no-access-error :user (ignore-errors
                                      (viewer-context-user vc))
                               :obj obj)))))

(defmethod can-edit :around (obj user)
  "In order to edit something, we also need to be able to view it."
  (and
   user
   (can-view obj user)
   (call-next-method)))
