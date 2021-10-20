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
