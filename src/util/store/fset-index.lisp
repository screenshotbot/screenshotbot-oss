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
                #:when-let
                #:curry)
  (:import-from #:util/store/store
                #:validate-index-values)
  (:import-from #:bknr.indices
                #:index-existing-error)
  (:export
   #:fset-set-index
   #:fset-unique-index
   #:fset-set-compat-index
   #:index-least
   #:fset-many-to-many-index))
(in-package :util/store/fset-index)

(defvar *index-id* 0)

(defclass abstract-fset-index ()
  ((slot-name :initarg :slots
              :initform nil
              :accessor %slots)
   (map :initform (fset:empty-map)
        :accessor %map)
   (index-id :initform #+lispworks (atomics:atomic-incf *index-id*) #-lispworks 0
             :reader index-id
             :documentation "A unique index to help debug index failures")))

(defmethod print-object ((self abstract-fset-index) out)
  (format out
          "#<~a ~a on ~a size {~d}>"
          (string (type-of self))
          (index-id self)
          (%slots self)
          (fset:size (%map self))))

(defmacro update-map (self (map) &body expr)
  `(atomics:atomic-update
    (slot-value ,self 'map)
    (lambda (,map)
      ,@expr)))

(defun %slot-name (self)
  (car (%slots self)))

(defmethod initialize-instance :after ((self abstract-fset-index)
                                       &rest args
                                       &key slot-name slots)
  (assert (not (and slot-name slots)))
  (log:debug "Created new index: ~a" self)
  (if slot-name
      (setf (%slots self) (list slot-name))))

(defmethod index-reinitialize :before ((new-index abstract-fset-index) old-index)
  (log:debug "Reinitializing index ~a from ~a" new-index
             old-index))

(defmethod index-reinitialize ((new-index abstract-fset-index)
                               (old-index abstract-fset-index))
  (update-map new-index (map)
    (declare (ignore map))
    (fset:map-union
     map
     (%map old-index)
     (curry #'merge-map-values new-index)))
  new-index)


(define-condition index-values-dont-match (warning)
  ((val1 :initarg :val1)
   (val2 :initarg :val2))
  (:report (lambda (w out)
             (with-slots (val1 val2) w
               (format out "Reinitializing index with values that don't match: ~a, ~a"
                       val1 val2)))))

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

(defclass fset-many-to-many-index (fset-set-index)
  ()
  (:documentation "An index where a slot can have multiple values, and you can
look up every record that has one of those slot values."))

(defclass fset-set-compat-index (fset-set-index)
  ()
  (:documentation "Like fset-set-index, but behaves similarly to hash-index. For example,
the index reader returns a list in reverse sorted order instead of a set."))

(defmethod index-object-key ((self abstract-fset-index) obj)
  (if (= 1 (length (%slots self)))
      (slot-value obj (%slot-name self))
      (mapcar (curry #'slot-value obj)
              (%slots self))))

(defmethod index-add :around ((self abstract-fset-index) obj)
  (when (and
         (every (curry #'slot-boundp obj) (%slots self))
         (index-object-key self obj))
    (call-next-method)))

(defmethod index-remove :around ((self abstract-fset-index) obj)
  (when (and
         (every (curry #'slot-boundp obj) (%slots self))
         (index-object-key self obj))
    (call-next-method)))

(defmethod merge-map-values ((self fset-set-index)
                             val1 val2)
  (fset:union val1 val2))

(defmethod merge-map-values ((self fset-unique-index)
                             val1 val2)
  (unless (eql val1 val2)
   (warn 'index-values-dont-match))
  val1)

(defmethod index-add ((self fset-unique-index)
                      obj)
  (update-map self (map)
    (let ((key (index-object-key self obj)))
      (cond
        ((when-let ((prev (fset:lookup map key)))
           (not (eql prev obj)))
         (error 'index-existing-error
                :index self
                :key key
                :value obj))
        (t
         (fset:with map
                    key
                    obj))))))

(defmethod index-add ((self fset-many-to-many-index)
                      obj)
  (update-map self (map)
    (reduce
     (lambda (map key)
       (fset:with map
                  key
                  (fset:with
                   (fset:lookup map key)
                   obj)))
     (index-object-key self obj)
     :initial-value map)))

(defmethod index-add ((self fset-set-index)
                      obj)
  (let ((key (index-object-key self obj)))
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
    (let ((key (index-object-key self obj)))
      (cond
        ((eql obj (fset:lookup map key))
         (fset:less map key))
        (t
         map)))))

(defmethod index-remove ((self fset-many-to-many-index)
                         obj)
  (update-map self (map)
    (reduce
     (lambda (map key)
       (fset:with map
                  key
                  (fset:less
                   (fset:lookup map key)
                   obj)))
     (index-object-key self obj)
     :initial-value map)))

(defmethod index-remove ((self fset-set-index) obj)
  (let ((key (index-object-key self obj)))
    (update-map self (map)
      (let ((new-val (fset:less
                      (fset:lookup map key)
                      obj)))
        (cond
          ((fset:empty? new-val)
           (fset:less map key))
          (t
           (fset:with map key new-val)))))))

(defmethod index-least ((self fset-set-index))
  (let ((set (nth-value 1 (fset:least (%map self)))))
    (when set
     (fset:least set))))

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

(define-condition corrupted-index (error)
  ((index :initarg :index)
   (all-elts :initarg :all-elts)
   (slot-name :initarg :slot-name)
   (actual-map :initarg :actual-map)
   (expected-map :initarg :expected-map))
  (:report (lambda (e out)
             (with-slots (index) e
               (format out "fset index does not match on ~a"
                       index)))))

(defmethod validate-index-values ((index abstract-fset-index) all-elts slot-name)
  (declare (optimize (debug 3) (speed 0)))
  (let ((tmp (make-instance (type-of index) :slots (list slot-name))))
    (loop for elt in all-elts
          do (index-add tmp elt))
    (unless (fset:equal? (%map tmp)
                         (%map index))
      (restart-case
          (error 'corrupted-index
                 :index index
                 :all-elts all-elts
                 :slot-name slot-name
                 :actual-map (%map index)
                 :expected-map (%map tmp))
        (util/store/store::fix-the-index ()
          (setf (%map index) (%map tmp)))))))
