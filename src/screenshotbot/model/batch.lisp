;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/model/batch
  (:use #:cl)
  (:import-from #:bknr.datastore
                #:persistent-class
                #:store-object)
  (:import-from #:util/store/store
                #:defindex
                #:with-class-validation)
  (:import-from #:util/store/fset-index
                #:fset-set-index
                #:fset-unique-index)
  (:import-from #:bknr.indices
                #:index-get)
  (:import-from #:util/store/object-id
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
   #:finalize-batch))
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
(with-class-validation
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
     (state-invalidated-p :initform nil
                          :transient t
                          :accessor state-invalidated-p
                          :documentation "The current state was invalidated since the last push to remote (GitHub/Bitbucket etc.)."))
    (:metaclass persistent-class)))

(defmethod name (batch)
  ;; just convenience for some of the macro-expansions
  (batch-name batch))

(defindex +batch-item-index+
  'fset-set-index
  :slot-name 'batch)

(with-class-validation
  (defclass batch-item (store-object)
    ((batch :initarg :batch
            :reader batch
            :index +batch-item-index+
            :index-reader batch-items)
     (%status :initarg :status
              :accessor batch-item-status)
     (%channel :initarg :channel
               :initform nil
               :accessor batch-item-channel)
     (%acceptable :initarg :acceptable
                  :accessor acceptable)
     (%title :initarg :title
             :initform ""
             :accessor batch-item-title)
     (%run :initarg :run
           :accessor batch-item-run)
     (%report :initarg :report
              :initform nil
              :accessor batch-item-report)
     ;; Oops, accident. Leaving it around to be safe. Can be cleared
     ;; in the future before a restart.
     (lock :transient t)
     (push-lock :transient t))
    (:metaclass persistent-class)))

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

(defmethod can-view ((self batch) user)
  (can-view (company self) user))

(defgeneric finalize-batch (batch)
  (:documentation "Called when the batch is finalized, typically because the commit is finalized. See implementation in batch-promoter"))

(defmethod promotion-log ((batch batch))
  (make-instance 'transient-promotion-log
                 :oid-array (oid-array batch)))
