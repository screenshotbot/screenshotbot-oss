;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(uiop:define-package :util/form-state
  (:nicknames #:jsearch/form-state)
  (:use #:cl
        #:alexandria)
  (:import-from #:closer-mop
                #:class-direct-slots
                #:slot-definition-name
                #:validate-superclass
                #:standard-direct-slot-definition
                #:direct-slot-definition-class)
  (:export
   #:form-state-class
   #:form-state-initargs
   #:read-form-state
   #:args-list-from-state
   #:form-state-validate
   #:make-form-state
   #:form-state-apply-edits))
(in-package :jsearch/form-state)

(defclass form-state-direct-slot-definition (standard-direct-slot-definition)
  ((transient :initarg :transient
              :initform nil
              :reader slot-transient-p)
   (mapped-to :initarg :mapped-to
              :initform nil
              :reader slot-mapped-to)
   (mapped-to-initarg :initarg :mapped-to-initarg
                      :initform nil
                      :reader slot-mapped-to-initarg)
   (mapped-via :initarg :mapped-via
               :initform #'identity
               :reader slot-mapped-via)
   (mapped-back-via :initarg :mapped-back-via
                    :initform #'identity
                    :reader slot-mapped-back-via)))

(defclass form-state-class (standard-class)
  ())

(defmethod validate-superclass ((class form-state-class) (superclass standard-class))
  t)

(defmethod direct-slot-definition-class ((class form-state-class)
                                         &key &allow-other-keys)
  'form-state-direct-slot-definition)

(defmethod form-state-initargs (form-state)
  (loop for slot in (closer-mop:class-direct-slots (class-of form-state))
        for name = (closer-mop:slot-definition-name slot)
        for mapped-to-initarg = (or
                                 (slot-mapped-to-initarg slot)
                                 (intern (string name) "KEYWORD"))
        if (not (slot-transient-p slot))
        appending
        (list mapped-to-initarg
              (funcall (slot-mapped-via slot)
                       (slot-value form-state name)))))

(defun read-form-state (class-name)
  (let ((state (make-instance class-name)))
    (loop for slot in (closer-mop:class-direct-slots (find-class class-name))
          for name = (closer-mop:slot-definition-name slot)
          for val = (hunchentoot:parameter (string-downcase name))
          do

             (setf (slot-value state name)
                   (if (eql 'number (closer-mop:slot-definition-type slot))
                       ;; What happens if it's invalid?
                       (if (str:emptyp val)
                           -1
                           (parse-integer val))
                       val)))
    state))

(defun safe-slot-mapped-to (slot)
  (or
   (slot-mapped-to slot)
   (slot-definition-name slot)))

(defmethod populate-form-state (form-state obj)
  (loop for slot in (class-direct-slots (class-of form-state))
        for name = (slot-definition-name slot)
        if (not
            (slot-transient-p slot))
        do
           (setf (slot-value form-state name)
                 (funcall
                  (slot-mapped-back-via slot)
                  (funcall (safe-slot-mapped-to slot)
                           obj)))))

(defmethod make-form-state (class-name obj &rest initargs)
  (let ((ret (apply #'make-instance class-name initargs)))
    (populate-form-state ret obj)
    ret))

(defmethod form-state-validate (state obj)
  nil)

(defmethod validate-slots (state)
  ;; todo: what kind of validations would actually make sense here?
  nil)

(defmethod form-state-validate :around (state obj)
  (append
   (validate-slots state)
   (call-next-method)))

(defun args-list-from-state (state)
  (loop for slot in (closer-mop:class-direct-slots (class-of state))
        for name = (closer-mop:slot-definition-name slot)
        collect
        (cons
         (intern (string-upcase name)  "KEYWORD")
         (slot-value state name))))

(defun original-slot-setter (slot)
  "Returns a funcation that can be called to set the value of the
  given slot in the *original* data object."
  (fdefinition `(setf ,(or
                        (slot-mapped-to slot)
                        (slot-definition-name slot)))))

(defmethod form-state-apply-edits (state obj)
  (loop for slot in (class-direct-slots (class-of state))
        for name = (slot-definition-name slot)
        if (not (slot-transient-p slot))
        do
           (funcall (original-slot-setter slot)
                    (funcall
                     (slot-mapped-via slot)
                     (slot-value state name))
                    obj)))
