;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/model/batch
  (:use #:cl)
  (:import-from #:bknr.datastore
                #:store-object-id
                #:class-instances
                #:persistent-class
                #:store-object)
  (:import-from #:util/store/store
                #:nsort-store-objects
                #:defindex
                #:with-class-validation)
  (:import-from #:util/store/fset-index
                #:fset-set-index
                #:fset-unique-index)
  (:import-from #:bknr.indices
                #:should-index-objects-for-class-p
                #:hash-index
                #:index-get)
  (:import-from #:util/store/object-id
                #:creation-time-from-oid
                #:oid-array
                #:find-by-oid
                #:object-with-oid)
  (:import-from #:screenshotbot/user-api
                #:can-view)
  (:import-from #:screenshotbot/model/recorder-run
                #:transient-promotion-log
                #:promotion-log
                #:recorder-run-repo-url
                #:phabricator-diff-id
                #:pull-request-url
                #:github-repo
                #:recorder-run-company)
  (:import-from #:auth/viewer-context
                #:logged-in-viewer-context)
  (:import-from #:screenshotbot/model/core
                #:ensure-slot-boundp)
  (:import-from #:util/store/store-migrations
                #:def-store-migration)
  (:import-from #:util/misc/lists
                #:with-batches)
  (:import-from #:util/cron
                #:def-cron)
  (:import-from #:util/store/simple-object-snapshot
                #:simple-object-snapshot)
  (:import-from #:util/store/store-version
                #:*snapshot-store-version*)
  (:import-from #:util/store/unlikely-to-change-snapshot
                #:unlikely-to-change-mixin
                #:*unlikely-to-change*)
  (:export
   #:find-or-create-batch
   #:batch-items
   #:batch-item-channel
   #:batch-name
   #:batch-item-run
   #:batch-item-report
   #:batch-commit
   #:batch-item-status
   #:batch-item-title
   #:lock
   #:push-lock
   #:state-invalidated-p
   #:finalize-batch
   #:batch))
(in-package :screenshotbot/model/batch)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun accessors ()
    '(company repo name phabricator-diff-id
      pull-request-url)))


(defindex +lookup-index-v4+
  'fset-set-index
  :slot-name '%commit)

;; Note that for most of the fields, we create an alias for the slot
;; readers to use the same slots as recorder-run as a convenience.
(with-class-validation ()
  (defclass batch (object-with-oid)
    ((%company :initarg :company
               :reader company
               :reader recorder-run-company)
     (%repo :initarg :repo
            :reader repo
            :reader github-repo
            :reader recorder-run-repo-url)
     (%commit :initarg :commit
              :index +lookup-index-v4+
              :index-reader batches-for-commit
              ;; Note: we don't need recorder-run-commit here because
              ;; a promoter should not be using that anyway. Promoters
              ;; should instead use CHECK-SHA.
              :reader batch-commit
              :reader commit)
     (%name :initarg :name
            :reader batch-name)
     (%phabricator-diff-id :initarg :phabricator-diff-id
                           :initform nil
                           :reader phabricator-diff-id)
     (%pull-request-url :initarg :pull-request-url
                        :initform nil
                        :reader pull-request-url)
     (lock :initform (bt:make-lock "batch-lock")
           :transient t
           :reader lock
           :documentation "A lock to make sure that ")
     (push-lock :initform (bt:make-lock "batch-push-lock")
                :transient t
                :reader push-lock
                :documentation "A lock to make sure only one promoter is updating the PR.")
     (state-invalidated-p :initform t
                          :transient t
                          :accessor state-invalidated-p
                          :documentation "The current state was invalidated since the last push to remote (GitHub/Bitbucket etc.)."))
    (:metaclass persistent-class)))

(defmethod name (batch)
  ;; just convenience for some of the macro-expansions
  (batch-name batch))

(defindex +batch-item-index+
  'hash-index
  :slot-name 'batch)

(with-class-validation ()
  (defclass batch-item (store-object
                        unlikely-to-change-mixin)
    ((batch :initarg :batch
            :reader batch
            :index +batch-item-index+
            :index-reader %batch-items)
     (%status :initarg :status
              :initform nil
              :accessor batch-item-status)
     (%channel :initarg :channel
               :initform nil
               :accessor batch-item-channel)
     (%acceptable :initarg :acceptable
                  :initform nil
                  :accessor acceptable
                  :documentation "This slot isn't actually being used (AFAICT)")
     (%title :initarg :title
             :initform ""
             :accessor batch-item-title)
     (%run :initarg :run
           :initform nil
           :accessor batch-item-run)
     (%report :initarg :report
              :initform nil
              :accessor batch-item-report)
     ;; Oops, accident. Leaving it around to be safe. Can be cleared
     ;; in the future before a restart.
     (lock :transient t)
     (push-lock :transient t))
    (:metaclass persistent-class)))

(defun batch-items (batch)
  "In the previous version of the index, we used an fset-index.

When reloading this code, the index isn't recreated. When rebooting,
it will recreate with with a hash-index.

This is for compatibility with the rest of the
code."
  (fset:convert 'fset:set
                (%batch-items batch)))

(defmethod bknr.datastore:make-object-snapshot-v2 ((self batch-item) next-object-id)
  (call-next-method))

(defvar *lock* (bt:make-lock))

(defun find-existing-batch #.`(&key commit
                                    ,@(accessors))
  (fset:do-set (batch (batches-for-commit commit))
    (when #.`(and
              ,@(loop for key in (accessors)
                      collect `(equal ,key (,key batch))))
          (return batch))))

(defun find-batches-for-commit (&key commit company)
  (fset:convert
   'list
   (fset:filter
    (lambda (batch)
      (eql (company batch) company))
    (batches-for-commit commit))))

(defun find-or-create-batch (&rest args
                             &key &allow-other-keys)
  (bt:with-lock-held (*lock*)
    (or
     (apply #'find-existing-batch args)
     (apply #'make-instance 'batch
            args))))


(defun find-batch-item (batch &key channel)
  (fset:do-set (item (batch-items batch))
    (when (eql (batch-item-channel item) channel)
      (return item))))

(defmethod auth:can-view ((self batch) user)
  (auth:can-view-with-normal-viewer-context
   user self))

(defmethod auth:can-viewer-view ((vc logged-in-viewer-context)
                                 (self batch))
  (auth:can-viewer-view vc (company self)))

(defgeneric finalize-batch (batch)
  (:documentation "Called when the batch is finalized, typically because the commit is finalized. See implementation in batch-promoter"))

(defmethod promotion-log ((batch batch))
  (make-instance 'transient-promotion-log
                 :oid-array (oid-array batch)))

(def-store-migration ("Ensure some slots are bound" :version 23)
  (ensure-slot-boundp 'batch-item '%status)
  (ensure-slot-boundp 'batch-item '%run))

(defmethod should-index-objects-for-class-p ((class (eql (find-class 'batch-item))))
  nil)

(defmethod class-instances ((name (eql 'batch-item)))
  (nsort-store-objects
   (loop for obj in (bknr.datastore:all-store-objects)
         if (typep obj 'batch-item)
           collect obj)))

(defun unchanged-batch-items (batch)
  "Items that only recorded \"No screenshots changed\" (or \"Nothing to
review\"). These never have a report, so deleting them only removes a
row pointing to the run from the batch page."
  (loop for item in (fset:convert 'list (batch-items batch))
        if (eql :success (batch-item-status item))
          collect item))

(defun delete-old-unchanged-batch-items (&key (days 90)
                                           (now (get-universal-time)))
  "Deletes the :SUCCESS items of every batch created more than DAYS
ago. Returns the number of deleted items.

If an old commit is re-run, its check is recomputed from the remaining
items. The status stays the same, since :SUCCESS has the lowest
priority, but the title and summary table may show fewer channels."
  (let ((cutoff (- now (* days 24 3600)))
        (deleted 0))
    (dolist (batch (class-instances 'batch))
      (when (< (creation-time-from-oid batch) cutoff)
        ;; Hold the lock so we don't delete an item that the promoter
        ;; has just found and is about to update.
        (bt:with-lock-held ((lock batch))
          (let ((items (unchanged-batch-items batch)))
            ;; The chunks keep us under CALL-ARGUMENTS-LIMIT.
            (with-batches (chunk items :batch-size 1000)
              (apply #'bknr.datastore:delete-objects chunk))
            (incf deleted (length items))))))
    (log:info "Deleted ~a old batch items" deleted)
    deleted))

;; Batch items are UNLIKELY-TO-CHANGE-MIXINs, so deleting one while a
;; background snapshot is running makes that snapshot fail. Snapshots
;; start at 6:00, and followers start theirs up to two hours later, so
;; stay well clear of that.
(def-cron delete-old-unchanged-batch-items (:minute 0 :hour 18)
  (delete-old-unchanged-batch-items))
