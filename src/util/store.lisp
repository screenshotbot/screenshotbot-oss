(in-package :util)

(defvar *object-store*)

(defclass safe-mp-store (bknr.datastore:mp-store)
  (lock))

(defmethod initialize-instance :after ((store safe-mp-store) &key directory &allow-other-keys)
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
                 :directory *object-store*
                 :subsystems (store-subsystems)))

(defun verify-store ()
  (let ((store-dir *object-store*))
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


(defun cron-snapshot ()
  (log:info "Snapshotting bknr.datastore")
  (snapshot))

(cl-cron:make-cron-job 'cron-snapshot
                        :minute 0
                        :step-hour 6
                        :hash-key 'cron-snapshot)
