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
  (:import-from #:screenshotbot/model/company
                #:company)
  (:export
   #:company-graph))
(in-package :screenshotbot/model/company-graph)

(defun reverse-graph ()
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
                       do
                          (push obj (gethash neighbor graph))
                          (vector-push-extend neighbor queue)))))
    graph))

(defmethod company-graph ((self company))
  (call-next-method))

(defmethod company-graph (self)
  "Get all objects belonging to an object, even though we call it company-graph"
  (let ((graph (reverse-graph)))
    (let ((seen (make-hash-table)))
      (labels ((dfs (obj)
                 (unless (gethash obj seen)
                   (setf (gethash obj seen) t)
                   (loop for neighbor in (gethash obj graph)
                         do (dfs neighbor)))))
        (dfs self)
        (loop for obj being the hash-keys of seen
              if (typep obj 'bknr.datastore:store-object)
                collect obj)))))
