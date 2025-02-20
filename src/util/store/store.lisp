;; Copyright 2018-Present Modern Interpreters Inc.
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/store/store
  (:nicknames :util/store)
  (:use #:cl
        #:bknr.datastore
        #:file-lock)
  (:import-from #:bknr.datastore
                #:deftransaction
                #:close-store)
  (:import-from #:easy-macros
                #:def-easy-macro)
  (:import-from #:bknr.datastore
                #:without-sync)
  (:import-from #:bknr.datastore
                #:store-transaction-log-stream)
  (:import-from #:bknr.datastore
                #:store-transaction-log-pathname)
  (:import-from #:bknr.datastore
                #:close-transaction-log-stream)
  (:import-from #:bknr.datastore
                #:close-transaction-log-stream)
  (:import-from #:file-lock
                #:make-file-lock)
  (:import-from #:bknr.datastore
                #:store-directory)
  (:import-from #:bknr.datastore
                #:restore-transaction-log)
  (:import-from #:alexandria
                #:when-let
                #:assoc-value)
  (:import-from #:bknr.indices
                #:*indexed-class-override*)
  (:import-from #:bknr.indices
                #:slot-index)
  (:import-from #:bknr.indices
                #:clear-slot-indices)
  (:import-from #:util/lists
                #:head)
  #+bknr.cluster
  (:import-from #:bknr.cluster/store
                #:on-snapshot-save-impl
                #:cluster-store-mixin
                #:backward-compatibility-mixin)
  #+bknr.cluster
  (:import-from #:bknr.cluster/server
                #:on-snapshot-load
                #:log-transaction-error
                #:data-path
                #:leaderp
                #:with-leadership-priority)
  (:import-from #:util/cron
                #:cron-enabled-on-store-p)
  (:import-from #:util/threading
                #:make-thread
                #:ignore-and-log-errors
                #:with-extras
                #:log-sentry)
  (:import-from #:util/misc
                #:safe-ensure-directories-exist)
  (:import-from #:util/events
                #:with-tracing)
  #+bknr.cluster
  (:import-from #:util/store/clone-logs-store
                #:clone-logs-store)
  (:import-from #:serapeum
                #:collecting)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:prepare-store-for-test
   #:prepare-store
   #:verify-store
   #:add-datastore-hook
   #:object-store
   #:safe-mp-store
   #:with-test-store
   #:*object-store*
   #:store-subsystems
   #:validate-indices
   #:register-ref
   #:find-any-refs
   #:safe-snapshot
   #:defindex
   #:validate-class
   #:with-class-validation
   #:def-store-local
   #:location-for-oid
   #:add-datastore-cleanup-hook
   #:*snapshot-hooks*
   #:raft-store
   #:fix-the-index
   #:make-default-store
   #:raft-store-final))
(in-package :util/store)

(defvar *object-store*)

(defvar *datastore-hooks* nil)

(defvar *datastore-cleanup-hooks* nil)

(defvar *calledp* nil)

(defvar *snapshot-hooks* nil
  "A snapshot hook is called with two arguments: the store, and the path
to the directory that was just snapshotted.")

(defparameter *enable-txn-log-lock* t)

(defun add-datastore-hook (fn &key immediate)
  "Add a hook, if :immediate is set, and the store is already active the
 callback is called immediately."
  (cond
   ((not *calledp*)
    (pushnew fn *datastore-hooks*))
   (immediate
    (funcall fn))))

(defun add-datastore-cleanup-hook (fn)
  (pushnew fn *datastore-cleanup-hooks*))

(defun dispatch-datastore-hooks ()
  (mapc 'funcall *datastore-hooks*)
  (setf *calledp* t))

(defun dispatch-datastore-cleanup-hooks ()
  (mapc 'funcall *datastore-cleanup-hooks*))

(defvar *ensure-directories-cache* (fset:empty-set))

(defun fast-ensure-directories-exist (path)
  (let ((dir (pathname-directory path)))
    (cond
      ((fset:@ *ensure-directories-cache* dir)
       path)
      (t
       (prog1
           (safe-ensure-directories-exist path)
         (setf *ensure-directories-cache* (fset:with *ensure-directories-cache*
                                                     dir)))))))

(defun object-store ()
  (let* ((dir *object-store*)
         (dir (if (or
                   (str:ends-with-p "/" dir)
                   (and (uiop:os-windows-p) (str:ends-with-p "\\" dir)))
                  dir (format nil "~a/" dir))))
   (let ((path (pathname dir)))
     (fast-ensure-directories-exist path)
     path)))

(defclass checksumed-mp-store ()
  ())

(defclass common-mp-store (bknr.datastore:mp-store)
  ())

(defclass safe-mp-store (common-mp-store
                         checksumed-mp-store)
  ((transaction-log-lock :initform nil)))

#+bknr.cluster
(defclass base-raft-store (util/store/elb-store:elb-store-mixin
                           clone-logs-store)
  ())

#+bknr.cluster
(defclass raft-store (backward-compatibility-mixin
                      with-leadership-priority
                      cluster-store-mixin
                      ;; Hopefully braft takes care of the locking here
                      bknr.datastore:store
                      base-raft-store)
  ())

(defclass raft-store-final (with-leadership-priority
                            cluster-store-mixin
                            ;; Hopefully braft takes care of the locking here
                             bknr.datastore:store
                             base-raft-store)
  ()
  (:documentation "The final raft store we'll be using")
  (:default-initargs :election-timeout-ms 2000))

(defclass ec2-store (raft-store-final)
  ()
  (:documentation "A store that's just configured with hostnames and group names, and
uses sane defaults."))

(defun ec2-get-local-ipv4 ()
  (str:trim
   (uiop:run-program "ec2metadata --local-ipv4"
                     :output 'string)))

(defmethod bknr.datastore::snapshot-store :around ((store base-raft-store))
  (let ((start-time (local-time:now)))
    (with-extras (("snapshot-start-time" start-time))
      (call-next-method))))

(defmethod initialize-instance :around ((self ec2-store) &rest args &key ips group port &allow-other-keys)
  (apply
   #'call-next-method
   self
   :data-path (format nil "/home/~a/raft-data/" group)
   :ip (ec2-get-local-ipv4)
   :priority
   (cond
     ((equal (first ips) (ec2-get-local-ipv4))
      1)
     (t
      0))
   :config (str:join ","
                     (loop for ip in ips
                           collect (format nil "~a:~a:0" ip port)))
   :group group
   :port port
   args))


(defmethod on-snapshot-load :after ((store base-raft-store) snapshot-reader)
  #+lispworks
  (progn
    (log:info "Running GC :coalesce")
    (hcl:gc-generation 4 :coalesce t)))


#+bknr.cluster
(defmethod log-transaction-error ((sm base-raft-store) trans e)
  (log:info "logging issue to sentry")
  (with-extras (("transaction" trans))
    (log-sentry e)))

(defclass store-for-test (common-mp-store)
  ())

(defmethod initialize-instance :before ((store safe-mp-store) &key directory &allow-other-keys))

(defun ensure-transaction-log-lock (store)
  (with-slots (transaction-log-lock) store
    (unless transaction-log-lock
      (when *enable-txn-log-lock*
        (log:info "Opening transaction log lock")
        (setf transaction-log-lock
              (make-file-lock
               :file
               (ensure-directories-exist
                (make-pathname
                 :type "lock"
                 :defaults
                 (store-transaction-log-pathname store)))))))))

(defun clear-transaction-log-lock (store)
  (with-slots (transaction-log-lock) store
    (when transaction-log-lock
      (log:info "Closing transaction log lock")
      (restart-case
          (release-file-lock transaction-log-lock)
        (ignore-release-file-lock ()
          (values)))
      (setf transaction-log-lock nil))))

(defmethod restore-transaction-log :before ((store safe-mp-store)
                                            transaction-log
                                            &key until)
  (declare (ignore until))
  (ensure-transaction-log-lock store))


(defmethod bknr.datastore::close-store-object :before ((store safe-mp-store))
  (dispatch-datastore-cleanup-hooks))

(defmethod bknr.datastore::close-store-object :after ((store safe-mp-store))
  (clear-transaction-log-lock store))

(defvar *subsystems* `((bknr.datastore:store-object-subsystem 10)
                       (bknr.datastore:blob-subsystem 11)))

(defmacro defsubsystem (name &key (priority 15))
  `(progn
     (setf (assoc-value *subsystems* ',name)
           (list ,priority))))

(defun store-subsystems ()
  (mapcar #'make-instance
          (mapcar #'first
                  (sort (copy-list *subsystems*) #'< :key #'second))))


(defun prepare-store-for-test (&key (dir "~/test-store/")
                                 (store-class 'store-for-test))
  (assert store-class)
  (make-instance store-class
                 :directory dir
                 :subsystems (store-subsystems)))

(def-easy-macro with-test-store (&key (globally nil)
                                      (store-class 'store-for-test)
                                      dir
                                      &fn body)
  (%%call-with-test-store body :globally globally :store-class store-class
                               :dir dir))

(def-easy-macro with-snapshot-lock (store  &fn fn)
  (log:info "Waiting for store snapshot lock")
  (let ((file-lock (make-file-lock
                    :file (path:catfile
                           (store-directory store)
                           "snapshot.lock")
                    ;; Fail immediately if we can't get the lock
                    :timeout -10)))
    (unwind-protect
         (funcall fn)
      (release-file-lock file-lock))))

(defun %%call-with-test-store (fn &key (globally nil)
                                store-class
                                dir)
  (when (boundp 'bknr.datastore:*store*)
    (error "Don't run this test in a live program with an existing store"))
  (flet ((inner-work ()
           (labels ((all-objects ()
                      (bknr.datastore:all-store-objects))
                    (maybe-ensure-empty ()
                      (unless dir
                        (assert (null (all-objects)))))
                    (call-with-dir (dir)
                      (prepare-store-for-test :dir dir :store-class store-class)
                      (setf util/store:*object-store* (namestring dir))
                      (assert bknr.datastore:*store*)
                      (maybe-ensure-empty)
                      (let ((classes-to-clean nil))
                        (unwind-protect
                             (progn
                               (without-sync ()
                                 (funcall fn))
                               #+nil
                               (let ((objs (all-objects)))
                                 (when objs
                                   (error "At the end of the test some objects were not deleted: ~s" objs))))
                          (clear-indices-for-tests)))))
             (cond
               (dir
                (call-with-dir dir))
               (t
                (tmpdir:with-tmpdir (dir)
                  (call-with-dir dir)))))))
    (cond
      (globally
       (unwind-protect
            (progn
              (inner-work))
         (makunbound 'util/store::*object-store*)
         (makunbound '*store*)))
      (t
       (let ((*store* nil)
             (util/store::*object-store* nil))
         (inner-work))))))

(defun clear-indices-for-tests ()
  (let ((classes-to-clean
          (fset:convert
           'fset:set
           (mapcar #'class-of (bknr.datastore:all-store-objects)))))
    (close-store)

    (fset:do-set (class classes-to-clean)
      (mapc
       #'clear-slot-indices
       (closer-mop:class-slots class)))))

(defun read-raft-config ()
  (let ((raft-config (path:catfile (object-store) "raft-config.lisp")))
    (when (path:-e raft-config)
      (read-from-string
       (uiop:read-file-string raft-config)))))

(defun make-default-store (&rest args)
  (apply #'make-instance
         (append
          args
          (list
           :directory (object-store)
           :subsystems (store-subsystems)))))

(defun prepare-store ()
  (let ((raft-config (read-raft-config)))
    (cond
      (raft-config
       (eval raft-config))
      (t
       (make-default-store 'safe-mp-store))))
  (dispatch-datastore-hooks))

(defun verify-store (&key (callback (lambda ())))
  (let ((store-dir (object-store)))
    (tmpdir:with-tmpdir (dir)
      (let ((out-current (path:catdir dir "current/")))
        (log:info "Copyin file ~a to ~a" store-dir dir)
        (copy-directory:copy (path:catdir store-dir "current/")
                             out-current)
        (assert (path:-d out-current))
        (let ((raft-config (read-raft-config)))
          (cond
            (raft-config
             (error "verify unimplemented yet."))
            (t
             (make-instance 'safe-mp-store
                            :directory dir
                            :subsystems (store-subsystems)))))
        (log:info "Got ~d objects" (length (bknr.datastore:all-store-objects)))
        (funcall callback)))))

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
  (let ((list (directory dir)))
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

(defun snapshot-timestamps-to-delete (timestamps &key (now (local-time:now))
                                                   (keep-all 14)
                                                   (keep-none 90))
  "Figures out which timestamps in the list of timestamps to delete.

NOW is the current timestamp, for testing.

KEEP-ALL indicates that all timestamps within those number of days will always be kept.

KEEP-NONE indicates that all timestamps beyond those days will always be deleted.

Between KEEP-NONE and KEEP-ALL, we'll keep at least one timestamp per
week, where weeks are calculated from 1-1-1990 (so each time this is
called, the week interval will be identical)"
  (assert (<= keep-all keep-none))
  (let ((timestamps (sort (copy-list timestamps)
                          #'local-time:timestamp>))
        ;; We number weeks based on days before NOW. This map keeps
        ;; track of weeks for which we've already accounted a snapshot
        ;; that we're keeping.
        (weeks-seen (make-hash-table)))
    (collecting
      (dolist (ts timestamps)
        (let ((week-num (floor (local-time:timestamp-difference ts (local-time:universal-to-timestamp 0))
                               (* 7 24 3600))))
          (cond
            ((local-time:timestamp>= ts (local-time:timestamp- now
                                                              keep-all :day))
             (values))
            ((local-time:timestamp<= ts (local-time:timestamp- now keep-none :day))
             (collect ts))
            ((not (gethash week-num  weeks-seen))
             (setf (gethash week-num weeks-seen) t))
            (t
             (collect ts))))))))

(defun delete-old-snapshots ()
  (ignore-and-log-errors ()
    (let* ((ts-dir (all-snapshots-sorted (object-store)))
           (timestamps (mapcar #'car ts-dir))
           (to-delete (snapshot-timestamps-to-delete timestamps)))
      (loop for ts in to-delete
            for dir = (assoc-value ts-dir ts :test #'local-time:timestamp=)
             do
                (delete-snapshot-dir dir)))))

#+bknr.cluster
(defmethod cron-enabled-on-store-p ((store cluster-store-mixin))
  (leaderp store))

(defun cron-snapshot ()
  (make-thread
   (lambda ()
    (when (and
           (boundp 'bknr.datastore:*store*))
      #+bknr.cluster
      (unless (leaderp bknr.datastore:*store*)
        ;; TODO: Coordinate snapshots via RPC from the leader, so that
        ;; we never have to worry about two snapshots happening at the
        ;; same time.
        (sleep (+ 300 (random (* 2 3600)))))
      (log:info "Snapshotting bknr.datastore")
      (safe-snapshot "cron-job" nil)))
   :name "snapshot-thread"))

(cl-cron:make-cron-job 'cron-snapshot
                        :minute 0
                        :hour 6
                        :hash-key 'cron-snapshot)

(defmethod all-subsystem-objects (subsystem)
  nil)

(defmethod all-subsystem-objects ((self store-object-subsystem))
  (bknr.datastore:class-instances 'store-object))

(defmethod all-global-objects (store)
  (loop for subsystem in (bknr.datastore::store-subsystems *store*)
        appending (all-subsystem-objects subsystem)))

(defmethod object-neighbors ((x store-object))
  "For a given object, find all neighboring objects. i,e, the objects
that this object directly references."
  (loop for slotd in (closer-mop:class-slots (class-of x))
        if (and
            (slot-boundp x (closer-mop:slot-definition-name slotd))
            (not (bknr.datastore::transient-slot-p slotd))
            (not (bknr.datastore::relaxed-object-reference-slot-p slotd)))
          collect
          (let ((slot-value
                  (slot-value x
                              (closer-mop:slot-definition-name slotd))))
            slot-value)))

(defmethod object-neighbors ((x cons))
  (loop for item on x
        if (consp item)
          collect (car item)
        else
          collect item))

(defmethod object-neighbors ((x null))
  nil)

(defmethod object-neighbors (x)
  nil)

(defmethod object-neighbors ((x vector))
  (loop for a across x
        collect a))

(defmethod object-neighbors ((x standard-object))
  (let ((slots (closer-mop:class-slots (class-of x))))
    (loop for slot in slots
          for slot-name = (closer-mop:slot-definition-name slot)
          if (slot-boundp x slot-name)
            collect (slot-value x slot-name))))

(defmethod find-any-refs (objects)
  "Similar to BKNR.DATASTORE:FIND-REFS, but all elements in the transitive paths from U - O to O.

This means you can find all unreferenced objects by doing
set-differences on O and the returned value from this."

  ;; Use this to stress-test find-any-refs in prod:
  ;; (length (find-any-refs (class-instances 'store-object)))

  (let ((original-objects (make-hash-table))
        (seen (make-hash-table))
        (ret (make-hash-table)))
    (loop for obj in objects do
      (setf (gethash obj original-objects) t))
    (labels ((dfs (x)
               (etypecase x
                 (standard-object
                  (unless (gethash x seen)
                    (setf (gethash x seen) t)
                    (when
                        ;; Notice that even if x is in original-objects,
                        ;; we must still traverse it, otherwise we
                        ;; might miss some paths.
                        (or
                         (loop for neighbor in (object-neighbors x)
                               if (dfs neighbor)
                                 collect t)
                         (gethash x original-objects))
                      ;; on the way back on the DFS, mark all the
                      ;; nodes as part of the result
                      (setf (gethash x ret) t)))
                  ;; But we still need to propagate the last cached
                  ;; result if we have already dfs-ed this node.
                  (gethash x ret))
                 (null
                  nil)
                 (cons
                  ;; Make sure we always travers in both paths. But we
                  ;; need tail call optimization, since the lists can
                  ;; be huge.
                  (labels ((cons-dfs (x default)
                             (typecase x
                               (cons
                                (let ((left (cons-dfs (car x) default)))
                                  (cons-dfs (cdr x) left)))
                               (otherwise
                                (or (dfs x) default)))))
                    (cons-dfs x nil)))
                 (vector
                  (loop for y across x
                        if (dfs y)
                          collect t))
                 (symbol nil)
                 (character nil)
                 (number nil)
                 (string nil))))
      (loop for x in (all-global-objects *store*)
            if (not (gethash x original-objects))
              do
                 (dfs x))
      (sort
       (loop for x being the hash-keys of ret
             collect x)
       #'< :key #'bknr.datastore:store-object-id))))

(defmethod bknr.datastore::snapshot-store :around ((store safe-mp-store))
  (with-snapshot-lock (store)
    (clear-transaction-log-lock store)
    (let ((ret (call-next-method)))
      (ensure-transaction-log-lock store)
      ret)))

(defmethod bknr.datastore::restore-store :around ((store safe-mp-store) &key until)
  (with-snapshot-lock (store)
    (call-next-method)))

(defun safe-snapshot (&optional (comment "default-comment") (force t))
  (safe-snapshot-impl *store* :comment comment
                      :force force))

(defmethod safe-snapshot-impl (store &key comment
                                       (force t))
  (log:info "Going into snapshot, write access will be locked")
  (time
   (snapshot)))

(defmethod safe-snapshot-impl ((store safe-mp-store) &key comment
                                                       (force t))
  (let ((directories (fad:list-directory *object-store*)))
    (call-next-method)
    (let* ((new-directories (fad:list-directory *object-store*))
           (directories (set-difference new-directories directories :test #'equal)))
      (unless (= 1 (length directories))
        (error "Expected to see one new directory, but saw: ~S" directories))
      (%write-comment (car directories) comment)
      (let ((dir (car directories)))
        (dispatch-snapshot-hooks dir)
        dir))))

(defun %write-comment (dir comment)
  (with-open-file (file (path:catfile dir "comment.txt")
                        :direction :output
                        ;; If we recovered from an existing
                        ;; snapshot, then the comment.txt might
                        ;; already be here.
                        :if-exists :supersede)
    (write-string comment file)))

(deftransaction fake-transaction ()
  nil)

#+bknr.cluster
(defmethod safe-snapshot-impl ((store cluster-store-mixin) &key comment
                                                             (force t))
  (when (and force #+bknr.cluster (leaderp store))
    ;; bknr.cluster will not take a snapshot if there have been no new
    ;; transactions. But if we're doing a reload from code, we
    ;; probably want to force a new snapshot.
    (fake-transaction))

  (call-next-method)
  (let ((dir (path:catdir (data-path store) "snapshot")))
    (%write-comment dir comment)
    (dispatch-snapshot-hooks dir)))

(defun dispatch-snapshot-hooks (dir)
  (loop for hook in *snapshot-hooks* do
    (log:info "Calling snapshot hook: ~a" hook)
    (funcall hook *store* dir)))

(defmethod slot-key-for-verification (metaclass slot)
  slot)

(defun verify-old-class (class-name slots metaclass)
  (flet ((key (x)
           (slot-key-for-verification metaclass x)))
   (let ((old-class (find-class class-name nil)))
     (when old-class
       (let ((old-slots
               (mapcar #'key
                       (mapcar #'closer-mop:slot-definition-name
                               (closer-mop:class-direct-slots old-class)))))
         (a:when-let ((diff (set-difference
                             old-slots
                             (mapcar #'key
                                     (mapcar #'car slots))
                             :test #'equal)))

           (cerror "Dangerous continue and lose data" "missing slots: ~a"
                   diff)))))))

(defmacro with-class-validation (&body body)
  "Wrap a defclass for store objects to add extra validations"
  (assert (= 1 (length body)))
  (let* ((def (car body))
         (class-name (elt def 1))
         (slots (elt def 3))
         (metaclass (loop for option in def
                          if (and
                              (listp option)
                              (eql :metaclass (first option)))
                            return (second option))))

    ;; verify both at compile time and load time, just how it is. The
    ;; compiler updates the classes unfortunately, which will make the
    ;; load-time verification pass.
    (verify-old-class class-name slots metaclass)

    `(progn
       (verify-old-class ',class-name
                         ',slots
                         ',metaclass)
       ,def)))

;; (validate-indices)

(defmacro defindex (name class &rest args &key slot-name
                                            slots
                    &allow-other-keys)
  (unless (or slots slot-name)
    (error "Must provide :slots or :slot-name"))
  (when (and slots slot-name)
    (error "Must provide only one of :slots or :slot-name"))
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (defvar ,name
       (make-instance ,class ,@args))))


(defvar *store-local-lock* (bt:make-lock))

(defvar *store-local-map* (make-hash-table
                           #+lispworks :weak-kind
                           #+lispworks :key)
  "A map from store to the alist of all store local variables")


(symbol-macrolet ((place (gethash bknr.datastore:*store* *store-local-map*)))
  (defun get-store-local (name)
    (util/misc:or-setf
     (a:assoc-value place name)
     (funcall (get name 'store-local-init-fn))
     :thread-safe t))
  (defun (setf get-store-local) (value name)
    (setf (a:assoc-value place name) value)))

(defmacro def-store-local (name initform &optional documentation)
  "Defines a variable thats local to the current store. You cannot use
this variable in LET forms, but you can SETF it if you like."
  `(progn
     (setf (get ',name 'store-local-init-fn)
           (lambda () ,initform))
     (define-symbol-macro
         ,name
         (get-store-local ',name))))

(defmethod location-for-oid ((root pathname) (oid array)
                             &key suffix
                               type)
  "Returns two values: the absolute pathname for the given oid and root,
 and the relative pathname relative to the object store. The second
 value is useful for propagating to s3-store."
  (let* ((oid (coerce oid '(vector (unsigned-byte 8))))
         (store-dir (bknr.datastore::store-directory
                                     bknr.datastore:*store*))
         (p1 (ironclad:byte-array-to-hex-string oid :start 0 :end 1))
         (p2 (ironclad:byte-array-to-hex-string oid :start 1 :end 2))
         (p4 (ironclad:byte-array-to-hex-string oid :start 2)))
    ;; The first two bytes change approximately once every 0.7 days,
    ;; so each directory has enough space for all files generated in
    ;; one day. That should be good enough for anybody!
    (let* ((relpath (make-pathname
                     :directory (list :relative p1 p2)
                     :type type
                     :name (cond
                             (suffix
                              (format nil "~a-~a" p4 suffix))
                             (t p4))))
           (file (path:catfile store-dir
                               root
                               relpath)))
      (fast-ensure-directories-exist file)
      (values file
              (namestring
               (path:catfile root relpath))))))

(defmethod fset:compare ((a store-object) (b store-object))
  (let ((*indexed-class-override* t))
    (fset:compare (store-object-id a) (store-object-id b))))

(deftransaction tx-force-error (msg)
  ;;(error "force crash with ~a" msg)
  )

(defun generate-sync-test (&optional (output *standard-output*))
  (dolist (subsystem (bknr.datastore::store-subsystems *store*))
    (generate-sync-test-for-subsystem *store* subsystem output)))

(defmethod generate-sync-test-for-subsystem (store subsystem output)
  (values))

(defmethod generate-sync-test-for-subsystem (store (subsystem store-object-subsystem) output)
  (dolist (obj (sort (copy-list (all-store-objects)) #'< :key #'store-object-id))
    (format output "~a " obj)
    (generate-sync-test-for-object obj output)
    (format output "~%"))
  (format output "~a~%" (bknr.datastore:class-instances 'bknr.datastore:store-object)))

(defmethod generate-sync-test-for-object (obj output))

(defmethod generate-sync-test-for-object :around ((obj store-object) output)
  (format output "id: ~a" (store-object-id obj))
  (call-next-method))

(defgeneric validate-index-values (index all-elts slot-name))

(defvar *profilep* nil)

(defmethod on-snapshot-save-impl :around ((store base-raft-store) snapshot-writer done)
  "Add timing for snapshots"
  (with-tracing (:snapshot)
    (cond
      #+lispworks
      (*profilep*
       (hcl:profile
        (call-next-method)))
      (t
       (call-next-method)))))

(defun find-deleted-references ()
  (let ((seen (make-hash-table))
        (queue (make-array 0 :adjustable t :fill-pointer t))
        (start 0))
    (loop for obj in (bknr.datastore:all-store-objects)
          do (vector-push-extend obj queue))
    (loop while (< start (length queue))
          do
             (let ((next (elt queue (1- (incf start)))))
               (unless (gethash next seen)
                 (setf (gethash next seen) t)
                 (block nil
                  (loop for neighbor in (object-neighbors next)
                        if (typep neighbor 'store-object)
                          do
                             (cond
                               ((bknr.datastore::object-destroyed-p neighbor)
                                (restart-case
                                    (error "Object ~a references a destroyed object ~a"  next neighbor)
                                  (delete-this-object ()
                                    (bknr.datastore:delete-object next)
                                    ;; We don't want to keep accessing this object
                                    (return))))
                               (t
                                (vector-push-extend neighbor queue))))))))))

(defvar *index-caches* (make-hash-table)
  "See T1322. Don't use this for anything. The main goal for this is to
hold on to a copy of the big indices in cases they get replaced, so
that we don't lose data and can restore ourself to a state where
snapshots can happen again")

(defun backup-indices ()
  (setf (gethash bknr.datastore::*id-index* *index-caches*) (get-universal-time))
  (setf (gethash bknr.datastore::*class-skip-index* *index-caches*) (get-universal-time)))

(cl-cron:make-cron-job
 'backup-indices
 :step-min 5
 :hash-key 'backup-indices)

