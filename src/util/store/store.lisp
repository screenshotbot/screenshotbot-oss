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
                #:cluster-store-mixin
                #:backward-compatibility-mixin)
  #+bknr.cluster
  (:import-from #:bknr.cluster/server
                #:log-transaction-error
                #:data-path
                #:leaderp
                #:with-leadership-priority)
  (:import-from #:util/cron
                #:cron-enabled-on-store-p)
  (:import-from #:util/threading
                #:with-extras
                #:log-sentry)
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
           (ensure-directories-exist path)
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
(defclass base-raft-store (util/store/elb-store:elb-store-mixin)
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
  (:documentation "The final raft store we'll be using"))


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

(defmethod maybe-gc-coalesce ((store safe-mp-store))
  #+lispworks
  (hcl:gc-generation 4 :coalesce t))

(defmethod restore-transaction-log :before ((store safe-mp-store)
                                            transaction-log
                                            &key until)
  (declare (ignore until))
  (maybe-gc-coalesce store)
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
                  (sort *subsystems* #'< :key #'second))))


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

(defun delete-old-snapshots ()
  ;; always keep at least 7 snapshots even if they are old
  (loop for (ts . dir) in (nthcdr 7 (all-snapshots-sorted (object-store)))
        if (local-time:timestamp< ts (local-time:timestamp- (local-time:now)
                                                            14 :day))
          do
             (delete-snapshot-dir dir)))

#+bknr.cluster
(defmethod cron-enabled-on-store-p ((store cluster-store-mixin))
  (leaderp store))

(defun cron-snapshot ()
  (when (boundp 'bknr.datastore:*store*)
    (log:info "Snapshotting bknr.datastore")
    (safe-snapshot "cron-job" nil)))

(cl-cron:make-cron-job 'cron-snapshot
                        :minute 0
                        :step-hour 6
                        :hash-key 'cron-snapshot)

(cl-cron:make-cron-job 'delete-old-snapshots
                       :minute 0
                       :hour 4
                       :hash-key 'delete-old-snapshots)

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
                       diff-1
                       diff-2)))))

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

(defvar *recent-validation-errors* nil)

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
      (restart-case
          (progn
            (format t "Total number of elements: ~d~%" (length all-elts))
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
           (sort x #'< :key #'store-object-id)))
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
  (defun validate-indices (&key (full nil))
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
  (when force
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

(defun verify-old-class (class-name slots)
  #+nil(log:info "Verifying: ~S: ~S " class-name slots)
  (let ((old-class (find-class class-name nil)))
    (when old-class
      (let ((old-slots (mapcar #'closer-mop:slot-definition-name
                               (closer-mop:class-direct-slots old-class))))
        (a:when-let ((diff (set-difference
                            old-slots
                            (mapcar #'car slots))))


          (cerror "Dangerous continue and lose data" "missing slots: ~a"
                  diff))))))

(defmacro with-class-validation (&body body)
  "Wrap a defclass for store objects to add extra validations"
  (assert (= 1 (length body)))
  (let* ((def (car body))
         (class-name (elt def 1))
         (slots (elt def 3)))
    ;; verify both at compile time and load time, just how it is. The
    ;; compiler updates the classes unfortunately, which will make the
    ;; load-time verification pass.
    (verify-old-class class-name slots)

    `(progn
       (verify-old-class ',class-name
                         ',slots)
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
  (fset:do-set (obj (fset:convert 'fset:set (all-store-objects)))
    (format output "~a " obj)
    (generate-sync-test-for-object obj output)
    (format output "~%")))

(defmethod generate-sync-test-for-object (obj output))
