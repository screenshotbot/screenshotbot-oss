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
                #:index-remove)
  (:import-from #:bknr.indices
                #:index-values)
  (:import-from #:bknr.indices
                #:slot-index)
  (:import-from #:alexandria
                #:curry)
  (:import-from #:util/store/store
                #:validate-index-values)
  (:import-from #:bknr.indices
                #:index-existing-error)
  (:export
   #:fset-set-index
   #:fset-unique-index
   #:fset-set-compat-index))
(in-package :util/store/fset-index)

(defclass abstract-fset-index ()
  ((slot-name :initarg :slots
              :reader %slots)
   (map :initform (fset:empty-map)
        :accessor %map)))

(defmacro update-map (self (map) &body expr)
  `(atomics:atomic-update
    (slot-value ,self 'map)
    (lambda (,map)
      ,@expr)))

(defun %slot-name (self)
  (car (%slots self)))

(defmethod index-reinitialize ((new-index abstract-fset-index)
                               (old-index abstract-fset-index))
  (update-map new-index (map)
    (declare (ignore map))
    (%map old-index))
  new-index)

(defmethod index-reinitialize ((new-index abstract-fset-index)
                               (old-index slot-index))
  (warn "Recreating index from slot-index")
  (mapcar (curry #'index-add new-index)
          (index-values old-index))
  new-index)


(defclass fset-unique-index (abstract-fset-index)
  ())

(defclass fset-set-index (abstract-fset-index)
  ((map :initform (fset:empty-map (fset:empty-set)))))

(defclass fset-set-compat-index (fset-set-index)
  ()
  (:documentation "Like fset-set-index, but behaves similarly to hash-index. For example,
the index reader returns a list in reverse sorted order instead of a set."))

(defmethod index-add :around ((self abstract-fset-index) obj)
  (when (and
         (slot-boundp obj (%slot-name self))
         (slot-value obj (%slot-name self)))
    (call-next-method)))

(defmethod index-add ((self fset-unique-index)
                      obj)
  (update-map self (map)
    (let ((key (slot-value obj (%slot-name self))))
      (cond
        ((fset:lookup map key)
         (error 'index-existing-error
                :index self
                :key key
                :value obj))
        (t
         (fset:with map
                    key
                    obj))))))

(defmethod index-add ((self fset-set-index)
                      obj)
  (let ((key (slot-value obj (%slot-name self))))
    (update-map self (map)
      (fset:with map
                 key
                 (fset:with
                  (fset:lookup map key)
                  obj)))))

(defmethod index-get ((self abstract-fset-index) key)
  (fset:lookup (%map self) key))

(defmethod index-clear ((self abstract-fset-index))
  (update-map self (map)
    (declare (ignore map))
    (fset:empty-map)))

(defmethod index-get :around ((self fset-set-compat-index) key)
  (reverse
   (fset:convert 'list (call-next-method))))

(defmethod index-clear ((self fset-set-index))
  (update-map self (map)
    (declare (ignore map))
    (fset:empty-map (fset:empty-set))))

(defmethod index-remove ((self fset-unique-index)
                         obj)
  (update-map self (map)
    (let ((key (slot-value obj (%slot-name self))))
      (cond
        ((eql obj (fset:lookup map key))
         (fset:less map key))
        (t
         map)))))

(defmethod index-remove ((self fset-set-index) obj)
  (let ((key (slot-value obj (%slot-name self))))
    (update-map self (map)
      (fset:with map
                 key
                 (fset:less
                  (fset:lookup map key)
                  obj)))))


(defmethod index-values ((self abstract-fset-index))
  (labels ((build-values (map result)
             (cond
               ((fset:empty? map)
                result)
               (t
                (multiple-value-bind (key val) (fset:arb map)
                  (build-values
                   (fset:less map key)
                   (fset:union
                    (%index-values-for-key self val)
                    result)))))))
    (fset:convert 'list (build-values (%map self) (fset:empty-set)))))

(defmethod %index-values-for-key ((self fset-unique-index) val)
  (fset:with (fset:empty-set) val))

(defmethod %index-values-for-key ((self fset-set-index) val)
  val)


(defmethod validate-index-values ((index abstract-fset-index) all-elts slot-name)
  (declare (optimize (debug 3) (speed 0)))
  (let ((tmp (make-instance (type-of index) :slots (list slot-name))))
    (loop for elt in all-elts
          do (index-add tmp elt))
    (unless (fset:equal? (%map tmp)
                         (%map index))
      (restart-case
          (error "fset index does not match")
        (fix-fset-index ()
          (setf (%map index) (%map tmp)))))))
