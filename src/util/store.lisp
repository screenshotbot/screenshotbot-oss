;; Copyright 2018-Present Modern Interpreters Inc.
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(in-package :util)

(defvar *object-store*)

(defvar *datastore-hooks* nil)
(defvar *calledp* nil)

(defun add-datastore-hook (fn)
  (unless *calledp*
   (pushnew fn *datastore-hooks*)))

(defun dispatch-datastore-hooks ()
  (mapc 'funcall *datastore-hooks*)
  (setf *calledp* t))

(defun object-store ()
  (let* ((dir *object-store*)
         (dir (if (str:ends-with-p "/" dir) dir (format nil "~a/" dir))))
   (let ((path (pathname dir)))
     (ensure-directories-exist path)
     path)))

(defclass safe-mp-store (bknr.datastore:mp-store)
  (lock))

(defmethod initialize-instance :before ((store safe-mp-store) &key directory &allow-other-keys)
  (with-slots (lock) store
    (setf lock
          (make-instance 'file-lock
                         :file (path:catfile directory
                                             "store.lock")))))

(defmethod bknr.datastore::close-store-object :after ((store safe-mp-store))
  (with-slots (lock) store
    (release-file-lock lock)))

(defun store-subsystems ()
  (list (make-instance 'bknr.datastore:store-object-subsystem)
        (make-instance 'bknr.datastore:blob-subsystem)))

(defun prepare-store-for-test ()
  (make-instance 'util:safe-mp-store
                 :directory "~/test-store/"
                 :subsystems (store-subsystems)))

(defun prepare-store ()
  (setf bknr.datastore:*store-debug* t)
  (make-instance 'util:safe-mp-store
                 :directory (object-store)
                 :subsystems (store-subsystems))
  (dispatch-datastore-hooks))

(defun verify-store ()
  (let ((store-dir (object-store)))
    (tmpdir:with-tmpdir (dir)
      (let ((out-current (path:catdir dir "current/")))
        (log:info "Copyin file ~a to ~a" store-dir dir)
        (uiop:run-program (list "rsync" "-av" (namestring (path:catdir store-dir "current/"))
                                (namestring out-current))
                          :output :interactive
                          :error-output :interactive)
        (assert (path:-d out-current))
        (make-instance 'util:safe-mp-store
                        :directory dir
                        :subsystems (store-subsystems))
        (log:info "Got ~d objects" (length (bknr.datastore:all-store-objects)))
        (log:info "Success!")))))

(defun parse-timetag (timetag)
  "timetag is what bknr.datastore calls it. See utils.lisp in
  bknr. This function converts the timetag into a valid timestamp."
  (multiple-value-bind (full parts)
      (cl-ppcre:scan-to-strings
       "(\\d\\d\\d\\d)(\\d\\d)(\\d\\d)T(\\d\\d)(\\d\\d)(\\d\\d)"
       timetag)
    (when full
     (apply #'local-time:encode-timestamp
            0 ;; nsec
            (reverse
             (loop for x across parts
                   collect (parse-integer x)))))))

(defun all-snapshots-sorted (dir)
  (let ((list (directory "/data/arnold/object-store/")))
    ;; remove any directories that don't look like timestamps
    (let ((list
            (loop for x in list
                  for dir-name = (car (last (pathname-directory x)))
                  for ts = (parse-timetag dir-name)
                  if ts
                    collect (cons ts x))))
      (sort
       list
       #'local-time:timestamp>
       :key 'car))))


(defun delete-snapshot-dir (dir)
  (assert (path:-d dir))
  (log:info "Deleting snapshot dir: ~a" dir)
  (fad:delete-directory-and-files dir))

(defun delete-old-snapshots ()
  ;; always keep at least 7 snapshots even if they are old
  (loop for (ts . dir) in (nthcdr 7 (all-snapshots-sorted (object-store)))
        if (local-time:timestamp< ts (local-time:timestamp- (local-time:now)
                                                            1 :month))
          do
             (delete-snapshot-dir dir)))


(defun cron-snapshot ()
  (log:info "Snapshotting bknr.datastore")
  (snapshot))

(cl-cron:make-cron-job 'cron-snapshot
                        :minute 0
                        :hour 6
                        :hash-key 'cron-snapshot)

(cl-cron:make-cron-job 'delete-old-snapshots
                       :minute 0
                       :hour 4
                       :hash-key 'delete-old-snapshots)

(defun build-hash-table (objects slot &key test)
  (let ((hash-table (make-hash-table :test test)))
    (loop for obj in objects
          if (slot-boundp obj slot)
          do
             (setf (gethash (slot-value obj slot) hash-table)
                   obj))
    hash-table))

(defun validate-class-index (class-name slot-name)
  (declare (optimize (debug 3)))
  (let* ((class (find-class class-name))
         (slot (loop for slot in (closer-mop:class-slots class)
                    if (eql slot-name (closer-mop:slot-definition-name slot))
                      return slot
                     finally (error "could not find slot")))
         (indices (bknr.indices::index-effective-slot-definition-indices slot)))
    (unless (= 1 (length indices))
      (cerror "Continue using the first index"
              "There are multiple indices for this slot (~a, ~a), probably an error: ~a"
              class-name
              slot-name
              indices))
    (let ((index (car indices)))
      (when (typep index 'bknr.indices:unique-index)
       (let* ((hash-table (bknr.indices::slot-index-hash-table index))
              (test (hash-table-test hash-table))
              (new-hash-table (build-hash-table (store-objects-with-class class-name)
                                                slot-name
                                           :test test)))
         (unless
             (equalp hash-table
                     new-hash-table)
           (cerror "Continue testing other indices" "The index is invalid for ~a, ~a"  class-name slot-name)))))))


(defun validate-indices ()
  (let* ((objects (bknr.datastore:all-store-objects))
         (classes (remove-duplicates (mapcar 'class-of objects))))
    (loop for class in classes
          collect
          (loop for slot in (closer-mop:class-slots class)
                for slot-name = (closer-mop:slot-definition-name slot)
                for indices = (cdr (bknr.indices::index-effective-slot-definition-indices slot))
                if (and indices
                        (not (eql 'bknr.datastore::id slot-name)))
                  do
                     (validate-class-index (class-name class)
                                           slot-name)))))

;; (validate-indices)
