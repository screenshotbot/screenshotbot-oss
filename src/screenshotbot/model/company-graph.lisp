;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/model/company-graph
  (:use #:cl)
  (:import-from #:util/store/store
                #:location-for-oid
                #:object-neighbors
                #:find-any-refs)
  (:import-from #:screenshotbot/model/user
                #:user-with-email
                #:adminp)
  (:import-from #:screenshotbot/model/note
                #:note)
  (:import-from #:screenshotbot/model/company
                #:company-with-name
                #:company)
  (:import-from #:bknr.datastore
                #:blob
                #:store-object-id
                #:blob-pathname
                #:*store*
                #:store-directory
                #:class-instances
                #:store-object-subsystem
                #:snapshot-subsystem-helper)
  (:import-from #:screenshotbot/model/image
                #:image-filesystem-pathname
                #:image)
  (:import-from #:util/copy-file
                #:copy-file-fast)
  (:import-from #:util/store/object-id
                #:oid
                #:oid-array)
  (:import-from #:util/misc
                #:safe-ensure-directories-exist)
  (:export
   #:company-graph
   #:company-full-graph)
  (:local-nicknames (#:image-comparison #:screenshotbot/model/image-comparison)))
(in-package :screenshotbot/model/company-graph)

(defvar *lparallelp* t)

(defvar *root-company* nil
  "The root company on which we are doing the current graph work. For
convenience.")

(defun pmapc (fn objs)
  (funcall (if *lparallelp* #'lparallel:pmapc #'mapc)
           fn objs))

(defun ignorable-atom (obj)
  "We know these objects can't eventually refer any other objects, so we
remove these from the graph."
  (or
   (stringp obj)
   (numberp obj)
   (symbolp obj)))

(defmethod object-neighbors-for-graph (x)
  (object-neighbors x))

(defmethod object-neighbors-for-graph ((note note))
  (unless (adminp (screenshotbot/model/note::user note))
    (call-next-method)))

(defmethod object-neighbors-for-graph ((self screenshotbot/model/screenshot::lite-screenshot))
  (list*
   (screenshotbot/model/image:find-image-by-oid (screenshotbot/model/screenshot::image-oid self))
   (call-next-method)))

(defun all-starting-store-objects ()
  (let ((all (bknr.datastore:all-store-objects)))
    (loop for obj in all
          if (not
              (or
               (typep obj 'gatekeeper/gatekeeper::gatekeeper)
               (typep obj 'gatekeeper/gatekeeper::access-control)))
            collect obj)))

(defun reverse-graph (&key (undirected nil))
  "Creates the graph as a hash-table."
  (let ((graph (make-hash-table)))
    (let ((seen (make-hash-table))
          (queue (make-array 0 :adjustable t :fill-pointer t))
          (start 0))
      (loop for obj in (all-starting-store-objects) do
        (vector-push-extend obj queue))
      (loop while (< start (length queue))
            for obj = (aref queue (1- (incf start)))
            do
               (unless (gethash obj seen)
                 (setf (gethash obj seen) t)
                 (loop for neighbor in (object-neighbors-for-graph obj)
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

(defmethod should-continue-traversing-p (obj neighbor)
  "Should we keep traversing when we're at this object?"
  t)

(defmethod should-continue-traversing-p ((x screenshotbot/model/screenshot-key::screenshot-key)
                                         neighbor)
  (and
   (listp neighbor)
   (every (alexandria:rcurry #'typep 'screenshotbot/model/image::mask-rect)
          neighbor)))

(defun find-reachable-store-objects (graph self)
  (let ((seen (make-hash-table))
        (queue (make-array 0 :adjustable t :fill-pointer t))
        (from (make-hash-table)) ;; where we came from, to generate a path
        (start 0))
    (vector-push-extend self queue)
    (loop while (< start (length queue))
          for obj = (aref queue (1- (incf start)))
          do
             (unless (gethash obj seen)
               (setf (gethash obj seen) t)
               (loop for neighbor in (gethash obj graph)
                     if (should-continue-traversing-p obj neighbor)
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
  (let ((*root-company* self))
   (let ((graph (reverse-graph :undirected t)))
     (find-reachable-store-objects graph self))))

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
    (save-objects objects file)))

(defun save-objects (objects file)
  (with-open-file (s file
                     :direction :output
                     :element-type '(unsigned-byte 8))
    (snapshot-subsystem-helper
     (loop for subsystem in (bknr.datastore::store-subsystems bknr.datastore:*store*)
           if (typep subsystem 'store-object-subsystem)
             return subsystem)
     s
     :map-store-objects (lambda (fn)
                          (mapc fn objects)))))

(defun save-images (objects &key output)
  (let ((images (loop for obj in objects
                      if (typep obj 'image)
                        collect obj))
        (image-blobs (ensure-directories-exist (path:catdir output "image-blobs/"))))
    (pmapc
     (lambda (img)
       (when (path:-e (image-filesystem-pathname img))
         (copy-file-fast (image-filesystem-pathname img)
                         (location-for-oid
                          image-blobs
                          (oid-array img))))
       (copy-image-cache img :output output))
     images)))

(defun copy-image-cache (img &key output)
  (dolist (size '("300x300" "600x600" "2000x2000"))
    (let ((args (list (oid img :stringp nil) :suffix size :type "webp")))
     (let ((src (apply #'location-for-oid #P "image-cache/" args))
           (dest (apply #'location-for-oid (path:catdir output "image-cache/") args)))
       (when (path:-e src)
         (copy-file-fast
          src
          (safe-ensure-directories-exist dest)))))))

(defun check-graph (objects)
  #+nil
  (loop for object in objects
        if (string-equal :log-file (type-of object))
          do (error "bad object: ~a" object)))

(auto-restart:with-auto-restart ()
 (defun copy-blobs (objects &key output)
   "As of now, this is mostly needed for commit-graphs"
   (check-graph objects)
   (let ((blob-root (ensure-directories-exist (path:catdir output "blob-root/"))))
     (pmapc
      (lambda (obj)
        (when (path:-e (blob-pathname obj))
          (log:info "Copying blob: ~a" obj)
          (uiop:copy-file
           (blob-pathname obj)
           (path:catfile blob-root (format nil "~a" (store-object-id obj))))))
      (loop for object in objects
            if (typep object 'blob)
              collect object)))))

(defun copy-keys (output)
  (dolist (key '("aes-128-key.txt" "blowfish-key.txt"))
    (uiop:copy-file
     (path:catfile (store-directory *store*) key)
     (path:catfile output key))))

(defun copy-other-snapshot-files (output)
  #+bknr.cluster
  (let ((snapshot-files
          (directory
           (path:catfile
            (bknr.cluster/server::data-path *store*)
            "snapshot/snapshot_*/"))))
   (loop for name in (list
                       "random-state"
                       "version-subsystem-snapshot")
         do
            (loop for src in snapshot-files
                  if (equal (pathname-name src) name)
                    do
                       (log:info "Copying ~a" src)
                       (uiop:copy-file src (path:catfile output "current/" name))))))

(defun save-image-comparison-snapshot (objects output)
  (let ((ht (make-hash-table)))
    (dolist (obj objects)
      (setf (gethash obj ht) t))
    (let ((res (fset:empty-set))j)
      (fset:do-set (imc image-comparison::*stored-cache*)
        (flet ((has? (x) (or
                          (not x)
                          (gethash x ht))))
          (when (and
                 (has? (image-comparison::image-comparison-before imc))
                 (has? (image-comparison::image-comparison-after imc))
                 ;; This is breaking thigs. But for now it ends up
                 ;; posting an empty image-comparison snapshot. Which
                 ;; is okay. In the future, it might be that the
                 ;; company graph is not going through these objects.
                 (has? (image-comparison::image-comparison-result imc)))
            (log:info "Copying: ~a" imc)
            (setf
             res (fset:with res imc)))))
      (log:info "Writing: ~a image-comparisons" (fset:size res))
      (image-comparison::write-snapshot output res))))

(defun save-graph-and-blobs (company &key output)
  (let ((company (typecase company
                   (string
                    (company-with-name company))
                   (t company))))
    (let ((objects (company-full-graph company)))
      (check-graph objects)
      (save-objects (copy-seq objects) (ensure-directories-exist
                               (path:catfile output "current/store-object-subsystem-snapshot")))
      (save-image-comparison-snapshot
       (copy-seq objects)
       (path:catfile output "current/image-comparison-subsystem-snapshot"))
      (log:info "Saving images")
      (save-images (copy-seq objects) :output output)
      (log:info "Saving blobs")
      (copy-blobs objects :output output)
      (copy-keys output)
      (copy-other-snapshot-files output))))


(defun parse-inconsistencies (msg)
  (let ((lines (str:lines msg)))
    (iter:iterate (iter:for line iter:in lines)
      (multiple-value-bind (match parts)
          (cl-ppcre:scan-to-strings
           "Reference to inexistent object with id (.*) from"
           line)
        (when match
          (iter:collect (bknr.datastore:store-object-with-id (parse-integer (elt parts 0)))))))))
