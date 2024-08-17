;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/store/permissive-persistent-class
  (:use #:cl)
  (:import-from #:bknr.datastore
                #:last-change
                #:id
                #:persistent-effective-slot-definition
                #:persistent-class)
  (:import-from #:bknr.indices
                #:destroyed-p
                #:clear-slot-indices)
  (:import-from #:util/store/store
                #:slot-key-for-verification)
  (:local-nicknames #+sbcl
                    (#:clos #:closer-mop))
  (:export
   #:permissive-persistent-class
   #:value-map))
(in-package :util/store/permissive-persistent-class)

(defclass base-permissive-persistent-object ()
  ())

(defclass permissive-persistent-class (persistent-class)
  ()
  (:documentation "A persistent-class, where the slots aren't stored in the :instance
storage. Instead the slot values are stored in a hash-map, where the
keys are (symbol-name symbol). This is 'permissive' in the sense that
you can change the name of the slot, and everything will still just
work."))

(defmethod closer-mop:compute-class-precedence-list ((class permissive-persistent-class))
  (let ((classes (reverse (call-next-method))))
    (pushnew (find-class 'base-permissive-persistent-object) classes)
    (reverse classes)))

(defclass value-map-slot (clos:standard-effective-slot-definition)
  ((original-slots :initarg :original-slots
                   :reader original-slots)))

(defun ignorable-slot-p (slot)
  "Slots for which we should just use the underlying non-virtual allocation"
  (member (clos:slot-definition-name slot)
          '(id last-change)))

(defmethod copy-slot (class (slot persistent-effective-slot-definition) &rest args)
  (apply #'make-instance 'persistent-effective-slot-definition
         (append
          args
          (list
           :class class
           :name (clos:slot-definition-name slot)
           :relaxed-object-reference (slot-value slot 'bknr.datastore::relaxed-object-reference)
           :transient (slot-value slot 'bknr.datastore::transient)
           :initform (clos:slot-definition-initform slot)
           :initfunction (clos:slot-definition-initfunction slot)
           :initargs (clos:slot-definition-initargs slot)
           :type (clos:slot-definition-type slot)
           :indices (slot-value slot 'bknr.indices::indices)))))

(defun should-original-slot-be-virtual-p (slot)
  (and
   (typep slot 'persistent-effective-slot-definition)
   (not (ignorable-slot-p slot))))

(defun find-value-map-slot (obj)
  (loop for slotd in (closer-mop:class-slots (class-of obj))
        if (typep slotd 'value-map-slot)
          return slotd))

(defun maybe-initialize-value-map (obj)
  (unless (slot-boundp obj 'value-map)
    (setf (slot-value obj 'value-map)
          (initialize-hash-table (original-slots (find-value-map-slot obj))))))

(defmethod clos:compute-slots ((class permissive-persistent-class))
  (let ((original-slots (call-next-method)))
    (append
     (loop for slot in original-slots
           if (should-original-slot-be-virtual-p slot)
             collect
             (copy-slot class slot
                        :allocation :virtual
                        :initform nil
                        :initfunction nil)
           else
             collect slot)
     (list
      (make-instance 'value-map-slot
                     :name 'value-map
                     :original-slots original-slots)))))

(defun initialize-hash-table (slots)
  (let ((res (make-hash-table :test #'equal)))
    (loop for slot in slots
          if (should-original-slot-be-virtual-p slot)
            do
               (let ((initfunction
                       (or
                        (clos:slot-definition-initfunction slot)
                        (when (clos:slot-definition-initform slot)
                          (lambda ()
                            (eval (clos:slot-definition-initform slot)))))))
                 (log:info "Calling initfunction ~a on ~a" slot initfunction)
                 (when initfunction
                   (setf (gethash (slot-key slot) res)
                         (funcall initfunction)))))
    res))

(defmethod clear-slot-indices ((slot value-map-slot))
  nil)

(defun slot-key-from-name (symbol)
  (let ((key (symbol-name symbol)))
    (cond
      ((str:starts-with-p "%" key)
       (str:substring 1 nil key))
      (t
       key))))

(defun slot-key (slot)
  (slot-key-from-name (clos:slot-definition-name slot)))

(defmethod (setf clos:slot-value-using-class) (new-value
                                               (class permissive-persistent-class)
                                               obj
                                               (slot persistent-effective-slot-definition))
  (cond
    ((ignorable-slot-p slot)
     (call-next-method))
    (t
     (setf (gethash (slot-key slot) (value-map obj))
           new-value))))

(defmethod clos:slot-value-using-class ((class permissive-persistent-class)
                                        obj
                                        (slot persistent-effective-slot-definition))
  (cond
    ((ignorable-slot-p slot)
     (call-next-method))
    (t
     (multiple-value-bind (res exists-p)
         (gethash (slot-key slot) (value-map obj))
       (cond
         (exists-p
          res)
         (t
          (error 'unbound-slot
                 :name (clos:slot-definition-name slot)
                 :instance obj)))))))

(defmethod value-map (self)
  (maybe-initialize-value-map self)
  (slot-value self 'value-map))

(defmethod clos:slot-boundp-using-class ((class permissive-persistent-class)
                                         obj
                                         (slot persistent-effective-slot-definition))
  (cond
    ((ignorable-slot-p slot)
     (call-next-method))
    (t
     (nth-value 1 (gethash (slot-key slot) (value-map obj))))))

(defmethod clos:slot-makunbound-using-class ((class permissive-persistent-class)
                                             obj
                                             #+lispworks
                                             (slot-name symbol)
                                             #-lispworks
                                             (slot persistent-effective-slot-definition))
  (let (#+lispworks (slot (clos:find-slot-definition slot-name class)))
   (cond
     ((ignorable-slot-p slot)
      (call-next-method))
     (t
      (remhash (slot-key slot) (value-map obj))))))

(defmethod slot-key-for-verification ((metaclass (eql 'permissive-persistent-class))
                                      slot)
  (slot-key-from-name slot))
