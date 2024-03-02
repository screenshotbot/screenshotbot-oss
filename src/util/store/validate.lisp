;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/store/validate
  (:use #:cl)
  (:import-from #:bknr.datastore
                #:store-objects-with-class
                #:store-object-id)
  (:import-from #:alexandria
                #:when-let)
  (:import-from #:util/lists
                #:head)
  (:import-from #:bknr.indices
                #:slot-index)
  (:import-from #:util/store/store
                #:validate-indices ;; To make it easier to deploy
                #:fix-the-index
                #:validate-index-values)
  (:local-nicknames (#:a #:alexandria)))
(in-package :util/store/validate)

(defvar *recent-validation-errors* nil)

(defun build-hash-table (objects slot &key test unique-index-p)
  (let ((hash-table (make-hash-table :test test)))
    (loop for obj in objects
          if (slot-boundp obj slot)
            do
               (let ((slot-value (slot-value obj slot)))
                 ;;(assert (not (eql :png slot-value)))
                 (cond
                   (unique-index-p
                    (when slot-value
                      (setf (gethash slot-value hash-table)
                            obj)))
                   (t
                    (when slot-value
                     (push obj (gethash slot-value hash-table)))))))
    hash-table))

(defun find-effective-slot (class slot-name)
  (loop for slot in (closer-mop:class-slots class)
        if (eql slot-name (closer-mop:slot-definition-name slot))
          return slot
        finally (error "could not find slot")))

(defun atomp (x)
  (or
   (null x)
   (not (listp x))))

(defun hash-set-difference (left right &key test)
  "Similar to set-"
  (let ((table (make-hash-table :test test)))
    (dolist (x left)
      (setf (gethash x table) t))
    (dolist (x right)
      (remhash x table))
    (alexandria:hash-table-keys table)))

(defun unordered-equalp (list1 list2 &key (test #'eql))
  (declare (optimize (debug 3)))
  (cond
    ((and (atomp list1)
          (atomp list2))
     (equal list1 list2))
    ((or (atomp list1)
         (atomp list2))
     ;; this could also be the case that one of the lists are nil, but
     ;; it's correct to send false in that case.
     nil)
    (t
     (let ((diff-1 (hash-set-difference list1 list2 :test test))
           (diff-2 (hash-set-difference list2 list1 :test test)))
       (values
        (and
         (eql nil diff-1)
         (eql nil diff-2))
        diff-1
        diff-2)))))


(define-condition hash-tables-keys-dont-match (error)
  ((keys1 :initarg :keys1)
   (keys2 :initarg :keys2)
   (diff-1 :initarg :diff-1)
   (diff-2 :initarg :diff-2))
  (:report (lambda (e out)
             (with-slots (diff-1 diff-2) e
               (format out "The two hash tables have different keys. ~%Missing keys in new-hash-table: ~s~% Missing keys in old hash-table: ~s~%"
                       (head diff-1 100)
                       (head diff-2 100))))))

(defun assert-hash-tables= (h1 h2)
  (unless (eql (hash-table-test h1)
               (hash-table-test h2))
    (error "the two hash tables have different test functions"))
  (let ((keys1 (alexandria:hash-table-keys h1))
        (keys2 (alexandria:hash-table-keys h2)))
    (multiple-value-bind (res diff-1 diff-2)
        (unordered-equalp
         keys1
         keys2
         :test (hash-table-test h1))
      (unless res
        (error 'hash-tables-keys-dont-match
               :keys1 keys1
               :keys2 keys2
               :diff-1 diff-1
               :diff-2 diff-2))))
  (loop for k being the hash-keys of h1
        for value1 = (gethash k h1)
        for value2 = (gethash k h2)
        do
           (multiple-value-bind (res diff-1 diff-2)
               (unordered-equalp  value1 value2)
             (unless res
               (error "the two hash tables have different values for key ~a~%Missing values in new hash-table:~S~%Missing values in old hash-table:~s" k diff-1 diff-2)))))


(defmethod validate-index-values :around (index all-elts slot-name)
  (handler-bind ((error (lambda (e)
                          (push e *recent-validation-errors*)
                          (setf *recent-validation-errors*
                                (head *recent-validation-errors* 10)))))
    (call-next-method)))

(defmethod validate-index-values ((index slot-index) all-elts
                                  slot-name)
  (let* ((unique-index-p (typep index 'bknr.indices:unique-index)))
    (let* ((hash-table (bknr.indices::slot-index-hash-table index))
           (test (hash-table-test hash-table))
           (new-hash-table (build-hash-table all-elts
                                             slot-name
                                             :test test
                                             :unique-index-p unique-index-p)))
      (format t "Total number of elements: ~d~%" (length all-elts))
      (restart-case
          (progn
            (assert-hash-tables= hash-table
                                 new-hash-table))
        (fix-the-index ()
          (setf (bknr.indices::slot-index-hash-table index)
                new-hash-table))))))

(defun validate-class-index (class-name slot-name)
  (declare (optimize (debug 3)))
  (format t "Testing ~a, ~a~%" class-name slot-name)
  (restart-case
      (let* ((class (find-class class-name))
             (slot (find-effective-slot class slot-name))
             (indices (bknr.indices::index-effective-slot-definition-indices slot)))
        (dolist (index indices)
          (let ((all-elts (store-objects-with-class class-name)))
            (handler-bind ((error (lambda (e)
                                    (declare (ignore e))
                                    (format t "Errors while processing index for ~a ~a ~a~%" class slot indices))))
              (restart-case
                  (validate-index-values index all-elts slot-name)
                (continue-testing-other-indices ()
                  (values)))))))
    (retry--validate-class-index ()
      (validate-class-index class-name slot-name))))

(defun all-store-objects-in-memory (&key full)
  (flet ((make-sorted (x)
           (sort (copy-list x) #'< :key #'store-object-id)))
    (let ((from-bknr (make-sorted (bknr.datastore:all-store-objects))))
      (cond
        ((null full)
         from-bknr)
        (t
         #-lispworks
         from-bknr
         #+lispworks
         (let ((rest nil))
           (hcl:sweep-all-objects
            (lambda (obj)
              (when (and
                     (typep obj 'store-object)
                     (not
                      ;; Under certain circumstances (it looks like when I
                      ;; update a class and there are deleted objects in
                      ;; memory? Not sure), object-destroyed-p can fail on
                      ;; actually destroyed objects. If that happens wrap
                      ;; this next part in an ignore-errors. I don't want
                      ;; to keep it by default since that's risky when
                      ;; going about rewriting existing indices.
                      (bknr.datastore::object-destroyed-p obj))
                     (ignore-errors
                      (store-object-id obj)))
                (push obj rest)))
            t)
           (let ((sorted (make-sorted rest)))
             (restart-case
                 (progn
                   (unless (equal sorted from-bknr)
                     (error "The objects in memory and bknr index is not in sync, ~a vs ~a objects"
                            (length sorted)
                            (length from-bknr)))
                   sorted)
               (return-the-list-from-memory ()
                 sorted)
               (return-the-list-from-bknr ()
                 from-bknr))))        )))))


(defun fast-remove-duplicates (list)
  (let ((hash-table (make-hash-table)))
    (loop for x in list
          do (setf (gethash x hash-table) t))
    (a:hash-table-keys hash-table)))

(auto-restart:with-auto-restart (:retries 3 :sleep 1)
  (defun validate-indices (&key (full nil) (fix-by-default nil))
    (flet ((work ()
             (let* ((objects (all-store-objects-in-memory :full full))
                    (classes (fast-remove-duplicates
                              (loop for class in (fast-remove-duplicates (mapcar 'class-of objects))
                                    append (closer-mop:class-precedence-list class)))))
               (log:info "Got ~a objects and ~a classes"
                         (length objects)
                         (length classes))
               (loop for class in classes
                     do
                        (loop for direct-slot in (closer-mop:class-direct-slots class)
                              for slot-name = (closer-mop:slot-definition-name direct-slot)
                              for slot = (find-effective-slot class slot-name)
                              if (or
                                  (bknr.indices::index-direct-slot-definition-index direct-slot)
                                  (bknr.indices::index-direct-slot-definition-index-type direct-slot))
                                if (not (eql 'bknr.datastore::id slot-name))
                                  do
                                     (let ((indices (bknr.indices::index-effective-slot-definition-indices slot)))
                                       (assert indices)
                                       (validate-class-index (class-name class)
                                                             slot-name))))
               t)))
      (cond
        (fix-by-default
         (handler-bind ((error (lambda (e)
                                 (when-let ((restart (find-restart 'fix-the-index)))
                                   ;; Note: log:info will not show up
                                   ;; in Capistrano output.
                                   (format t "Calling restart for ~a~%" e)
                                   (invoke-restart restart)))))
           (work)))
        (t
         (work))))))
