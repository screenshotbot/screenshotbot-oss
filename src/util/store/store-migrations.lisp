;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/store/store-migrations
  (:use #:cl)
  (:import-from #:easy-macros
                #:def-easy-macro)
  (:import-from #:alexandria
                #:when-let
                #:assoc-value)
  (:import-from #:bknr.datastore
                #:deftransaction)
  (:import-from #:util/store/store-version
                #:*snapshot-store-version*
                #:*store-version*)
  (:export
   #:def-store-migration
   #:run-migrations
   #:run-migrations-async
   #:migration-running-p
   #:migration-failed-p
   #:migration-error))
(in-package :util/store/store-migrations)

(defclass migration ()
  ((version :initarg :version)
   (name :initarg :name
         :reader name)
   (body :initarg :body
         :reader body)))

(defvar *migrations* nil)

(def-easy-macro def-store-migration (name &key version &fn fn)
  (setf
   (assoc-value *migrations* version)
   (make-instance 'migration
                  :version version
                  :name name
                  :body fn)))

(deftransaction set-snapshot-version (version)
  (setf *snapshot-store-version* version))

(defun run-migration-for-version (version)
  (format t "Current thread is: ~a~%" (bt:current-thread))
  (let ((migration (assoc-value *migrations* version)))
    (cond
      (migration
       (format t "Running migration: ~a~%" (name migration))
       (funcall (body migration)))
      (t
       (format t "No migrations to run for version: ~a~%" version)))))

(defun bump-version ()
  (let ((version *snapshot-store-version*))
    (assert (< version *store-version*))
    (let ((version (1+ version)))
      (run-migration-for-version version)
      (set-snapshot-version version))))

(defun needs-work-p ()
  (< *snapshot-store-version* *store-version*))

(defun run-migrations (&key (snapshot t))
  (when (needs-work-p)
    (format t "After running migrations (current version: ~a)~%" *snapshot-store-version*)
    (when snapshot
      (util/store:safe-snapshot
       (format nil "Before running migrations (current version: ~a)~%" *snapshot-store-version*)))
    (loop while (needs-work-p)
          do
             (progn
               (format t "Current store version is ~a~%" *snapshot-store-version*)
               (bump-version)))
    (format t "After running migrations (current version: ~a)~%" *snapshot-store-version*)
    (when snapshot
     (util/store:safe-snapshot
      (format nil "After running migrations (current version: ~a)~%" *snapshot-store-version*)))))

(def-store-migration ("Dummy migration for version test" :version 2)
  (log:info "Nothing to do in this migration"))

;;; Async migration support
(defvar *migration-thread* nil
  "The current migration thread, or NIL if no migration is running")

(defvar *migration-error* nil
  "If the migration failed, this will contain the error condition")

(defvar *migration-lock* (bt:make-lock "migration-lock")
  "Lock to protect migration state")

(defun migration-running-p ()
  "Returns T if a migration is currently running"
  (bt:with-lock-held (*migration-lock*)
    (and *migration-thread*
         (bt:thread-alive-p *migration-thread*))))

(defun migration-failed-p ()
  "Returns T if the last migration failed"
  (bt:with-lock-held (*migration-lock*)
    (not (null *migration-error*))))

(defun migration-error ()
  "Returns the error from the last failed migration, or NIL"
  (bt:with-lock-held (*migration-lock*)
    *migration-error*))

(defun run-migrations-async (&key (snapshot t))
  "Run migrations in a background thread. Returns immediately.
   Use migration-running-p to check if still running,
   migration-failed-p to check if it failed, and
   migration-error to get the error if it failed."
  (bt:with-lock-held (*migration-lock*)
    (when (and *migration-thread* (bt:thread-alive-p *migration-thread*))
      (error "A migration is already running"))
    (setf *migration-error* nil)
    (setf *migration-thread*
          (bt:make-thread
           (lambda ()
             (handler-case
                 (progn
                   (format t "Starting migrations in background thread~%")
                   (run-migrations :snapshot snapshot)
                   (format t "Migrations completed successfully~%"))
               (error (e)
                 (bt:with-lock-held (*migration-lock*)
                   (setf *migration-error* e))
                 (format t "Migration failed with error: ~a~%" e)
                 (trivial-backtrace:print-backtrace e))))
           :name "migration-thread")))
  (values))
