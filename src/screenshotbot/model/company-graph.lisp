;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/model/company-graph
  (:use #:cl)
  (:import-from #:util/store/store
                #:object-neighbors
                #:find-any-refs)
  (:import-from #:screenshotbot/model/user
                #:user-with-email)
  (:import-from #:screenshotbot/model/company
                #:company)
  (:import-from #:bknr.datastore
                #:store-object-subsystem
                #:snapshot-subsystem-helper)
  (:export
   #:company-graph
   #:company-full-graph))
(in-package :screenshotbot/model/company-graph)

(defun ignorable-atom (obj)
  "We know these objects can't eventually refer any other objects, so we
remove these from the graph."
  (or
   (stringp obj)
   (numberp obj)
   (symbolp obj)))

(defun reverse-graph (&key (undirected nil))
  "Creates the graph as a hash-table."
  (let ((graph (make-hash-table)))
    (let ((seen (make-hash-table))
          (queue (make-array 0 :adjustable t :fill-pointer t))
          (start 0))
      (loop for obj in (bknr.datastore:all-store-objects) do
        (vector-push-extend obj queue))
      (loop while (< start (length queue))
            for obj = (aref queue (1- (incf start)))
            do
               (unless (gethash obj seen)
                 (setf (gethash obj seen) t)
                 (loop for neighbor in (object-neighbors obj)
                       if (not (ignorable-atom neighbor))
                         do
                            (unless (or
                                     (typep neighbor 'bknr.datastore:store-object)
                                     (listp neighbor)
                                     (arrayp neighbor)
                                     (typep neighbor 'util/store/object-id:oid)
                                     (typep neighbor 'screenshotbot/model/screenshot::lite-screenshot))
                              (log:warn "found an object of weird type: ~a" neighbor))
                            (push obj (gethash neighbor graph))
                            (when undirected
                              (push neighbor (gethash obj graph)))
                            (vector-push-extend neighbor queue)))))
    graph))

(defmethod company-graph ((self company))
  (call-next-method))

(defun find-reachable-store-objects (graph self &key exclude)
  (let ((seen (make-hash-table))
        (queue (make-array 0 :adjustable t :fill-pointer t))
        (from (make-hash-table)) ;; where we came from, to generate a path
        (start 0))
    (loop for e in exclude
          do (setf (gethash e seen) t))
    (vector-push-extend self queue)
    (loop while (< start (length queue))
          for obj = (aref queue (1- (incf start)))
          do
             (unless (gethash obj seen)
               (setf (gethash obj seen) t)
               (loop for neighbor in (gethash obj graph)
                     do
                        (unless (gethash neighbor from)
                          (setf (gethash neighbor from) obj))
                        (vector-push-extend neighbor queue))))
    (values
     (loop for obj being the hash-keys of seen
           if (typep obj 'bknr.datastore:store-object)
             collect obj)
     from)))

(defmethod company-graph (self)
  "Get all objects belonging to an object, even though we call it company-graph"
  (let ((graph (reverse-graph)))
    (find-reachable-store-objects graph self)))

(defmethod company-full-graph (self)
  "Get all the objects that refer or are referenced by, directly or
indirectly to a company. This is useful for copying a company and its
objects to a new instances. To see why this is implemented this way,
see the full-graph-finds-everything test."
  (let ((graph (reverse-graph :undirected t)))
    (find-reachable-store-objects graph self :exclude
                                  (list (user-with-email "arnold@tdrhq.com")))))

(defun find-a-path (src dest)
  "Find a path from src to dest in the undirected full graph. For debugging"
  (let ((graph (reverse-graph :undirected t)))
    (multiple-value-bind (reach backlinks)
        (find-reachable-store-objects graph src)
      (declare (ignore reach))
      (loop for x = (gethash dest backlinks)
            while (and x (not (eql src x)))
            do
               (setf dest x)
               (log:info "<-- ~a" x)))))

(defun save-graph (company file)
  "Save all the objects related to a company to a snapshot. Useful for
moving a company to a new instance."
  (let ((objects (company-full-graph company)))
    (with-open-file (s file
                       :direction :output
                       :element-type '(unsigned-byte 8))
      (snapshot-subsystem-helper
       (loop for subsystem in (bknr.datastore::store-subsystems bknr.datastore:*store*)
             if (typep subsystem 'store-object-subsystem)
               return subsystem)
       s
       :map-store-objects (lambda (fn)
                            (mapc fn objects))))))