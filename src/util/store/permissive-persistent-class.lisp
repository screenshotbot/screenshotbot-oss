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
  (:local-nicknames #+sbcl
                    (#:clos #:closer-mop))
  (:export
   #:permissive-persistent-class
   #:value-map))
(in-package :util/store/permissive-persistent-class)

(defclass permissive-persistent-class (persistent-class)
  ()
  (:documentation "A persistent-class, where the slots aren't stored in the :instance
storage. Instead the slot values are stored in a hash-map, where the
keys are (symbol-name symbol). This is 'permissive' in the sense that
you can change the name of the slot, and everything will still just
work."))

(defclass value-map-slot (clos:standard-effective-slot-definition)
  ())

(defun ignorable-slot-p (slot)
  "Slots for which we should just use the underlying non-virtual allocation"
  (member (clos:slot-definition-name slot)
          '(id last-change)))

(defmethod clos:compute-slots ((class permissive-persistent-class))
  (let ((original-slots (call-next-method)))
    (list*
     (make-instance 'value-map-slot
                    :name 'value-map
                    :initform nil
                    :initfunction #'(lambda ()
                                      (make-hash-table :test #'equal)))
     (loop for slot in original-slots
           if (and
               (typep slot 'persistent-effective-slot-definition)
               (not (ignorable-slot-p slot)))
             do
                (setf (closer-mop:slot-definition-allocation slot) :virtual)
           finally
           (return original-slots)))))


(defmethod clear-slot-indices ((slot value-map-slot))
  nil)

(defmethod (setf clos:slot-value-using-class) (new-value
                                               (class permissive-persistent-class)
                                               obj
                                               (slot persistent-effective-slot-definition))
  (cond
    ((ignorable-slot-p slot)
     (call-next-method))
    (t
     (setf (gethash (string (clos:slot-definition-name slot)) (value-map obj))
           new-value))))

(defmethod clos:slot-value-using-class ((class permissive-persistent-class)
                                        obj
                                        (slot persistent-effective-slot-definition))
  (cond
    ((ignorable-slot-p slot)
     (call-next-method))
    (t
     (gethash (string (clos:slot-definition-name slot)) (value-map obj)))))

(defmethod value-map (self)
  (slot-value self 'value-map))

(defmethod clos:slot-boundp-using-class ((class permissive-persistent-class)
                                         obj
                                         (slot persistent-effective-slot-definition))
  (cond
    ((ignorable-slot-p slot)
     (call-next-method))
    (t
     (nth-value 1 (gethash (string (clos:slot-definition-name slot)) (value-map obj))))))
