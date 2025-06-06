;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(uiop:define-package :screenshotbot/model/commit-graph
  (:use #:cl #:alexandria)
  (:import-from #:bknr.datastore
                #:store-object
                #:class-instances
                #:deftransaction
                #:store-object-id
                #:store-objects-with-class
                #:blob-pathname
                #:persistent-class)
  (:import-from #:screenshotbot/git-repo
                #:commit-graph-dag
                #:find-or-create-commit-graph
                #:commit-graph)
  (:import-from #:screenshotbot/model/company
                #:company)
  (:import-from #:util/store
                #:with-class-validation)
  (:import-from #:util/cron
                #:def-cron)
  (:import-from #:alexandria
                #:when-let)
  (:import-from #:util/store/fset-index
                #:fset-set-index)
  (:import-from #:util/store/store
                #:defindex)
  (:import-from #:util/store/store-migrations
                #:def-store-migration)
  (:import-from #:util/events
                #:with-tracing)
  (:import-from #:screenshotbot/model/core
                #:ensure-slot-boundp)
  (:import-from #:util/store/simple-object-snapshot
                #:simple-object-snapshot
                #:snapshot-slot-value)
  (:import-from #:util/lists
                #:with-batches)
  (:import-from #:util/store/store-version
                #:*store-version*)
  (:import-from #:bknr.indices
                #:unique-index)
  (:import-from #:util/misc
                #:?.)
  (:export
   #:commit-graph
   #:repo-url
   #:lock
   #:find-commit-graph
   #:find-or-create-commit-graph
   #:commit-graph-dag))
(in-package :screenshotbot/model/commit-graph)

(defvar *flush-lock* (bt:make-lock "dag-flush-lock"))

(defvar *lock* (bt:make-lock))

(defun transient-dag-p ()
  "In older versions we used to read the DAG from a transient slot. In
this new version, we no longer use the transient slot. Eventually you
can delete this code. T1739"
  (< *store-version* 32))

(defindex +normalized-repo-url-index+
  'fset-set-index
  :slot-name '%normalized-repo-url)

(with-class-validation
  (defclass commit-graph (bknr.datastore:blob)
    ((url :type string
          :initform nil
          :accessor repo-url
          :initarg :url)
     (%normalized-repo-url :type string
                           :accessor normalized-repo-url
                           :index +normalized-repo-url-index+
                           :index-reader find-by-normalized-url
                           :initarg :normalized-url)
     (company :initarg :company
              :accessor company
              :initform nil)
     (lock :initform (bt:make-recursive-lock)
           :transient t
           :accessor lock)
     (dag :initform nil
          :transient t
          :accessor %commit-graph-dag)
     (refs :initform nil 
           :initarg :refs
           :accessor %commit-graph-refs
           :writer (setf commit-graph-refs)
           :documentation "An fset:map, with the last known refs. These refs
are force updated, so it might move `backwards`! The main purpose of these
refs is just to allow incremental updates, and for potential garbage
collection. For now, if you need a source of truth of refs, you might want to
use the refs in promoted runs.")
     (dag-v2 :accessor %persisted-dag
             :initarg :dag
             :documentation "Move from DAG from DAG-V2 to keep the DAG persisted.")
     (needs-flush-p :initform nil
                    :transient t
                    :accessor needs-flush-p))
    (:metaclass persistent-class)
    (:default-initargs :dag nil)))

(defmethod commit-graph-refs ((self commit-graph))
  (let ((refs (%commit-graph-refs self)))
    (when (listp refs) #| or nil |#
      (setf refs
            (setf (%commit-graph-refs self)
                  (fset:convert 'fset:map refs))))
    refs))

(defindex +normalization-override-from+
  'unique-index
  :test #'equal
  :slot-name '%from)

(defclass normalization-override (store-object)
  ((%from :initarg :from
          :index +normalization-override-from+
          :index-reader find-normalization-override)
   (%to :initarg :to
        :reader normalization-override-to))
  (:metaclass persistent-class)
  (:documentation "See T1748, specific issues with certain customers"))

(defmethod bknr.datastore:make-object-snapshot ((self commit-graph))
  (make-instance 'simple-object-snapshot
                 :object self
                 :except-slots '(dag-v2)))

(defmethod snapshot-slot-value ((self commit-graph) (slot (eql 'dag-v2)))
  (when-let ((dag (slot-value self 'dag-v2)))
   (dag:clone-dag dag)))

(defun normalize-url (repo-url)
  "Normalizing is relatively safe. Even if two distinct repos normalize
to the same repo, the graph will still be the same."
  (or
   (?. normalization-override-to (find-normalization-override repo-url))
   (let ((repo-url (str:downcase repo-url)))
     (flet ((remove-prefixes (repo-url)
              (let ((git-prefix "^(ssh[:/]//)?git@"))
                (cond
                  ((cl-ppcre:scan git-prefix repo-url)
                   (cl-ppcre:regex-replace-all
                    git-prefix (str:replace-all ":" "/" repo-url) "https://"))
                  (t repo-url)))))
       (let ((suffixes (list "/" ".git")))
         (loop for suffix in suffixes
               if (str:ends-with-p suffix repo-url)
                 return (normalize-url (str:substring 0 (- (length repo-url)
                                                           (length suffix))
                                                      repo-url))
               finally (return (remove-prefixes repo-url))))))))


(defmethod find-commit-graph ((company company) (url string))
  (log:info "Finding commit graph for company ~a and repo ~a" company url)
  (or
   (fset:do-set (cg (find-by-normalized-url (normalize-url url)))
     (when (eql company (company cg))
       (return cg)))
   (%find-by-unnormalized-url company url)))

(defun %find-by-unnormalized-url (company url)
  (loop for commit-graph in (store-objects-with-class 'commit-graph)
        if (and
            (eql (company commit-graph) company)
            (equal (repo-url commit-graph) url))
          do (return commit-graph)))

(let ((lock (bt:make-lock)))
  (defmethod find-or-create-commit-graph ((company company) (url string))
    (bt:with-lock-held (lock)
      (or
       (find-commit-graph company url)
       (make-instance 'commit-graph
                      :url url
                      :normalized-url (normalize-url url)
                      :company company)))))

(defmethod commit-graph-dag ((obj commit-graph))
  (cond
    ((transient-dag-p)
     (util:or-setf
      (%commit-graph-dag obj)
      (bt:with-recursive-lock-held ((lock obj))
        (cond
          ((not (path:-e (blob-pathname obj)))
           (make-instance 'dag:dag))
          (t
           (with-open-file (s (blob-pathname obj) :direction :input)
             (dag:read-from-stream s)))))))
    (t
     (commit-graph-persisted-dag obj))))

(defmethod commit-graph-persisted-dag ((obj commit-graph))
  "Eventually commit-graph-dag should point here."
  (util:or-setf
   (%persisted-dag obj)
   (make-instance 'dag:dag)
   :thread-safe t
   :lock *lock*))

(defmethod (setf commit-graph-dag) (dag (obj commit-graph))
  (cond
    ((transient-dag-p)
     (setf (%commit-graph-dag obj) dag)
     (setf (needs-flush-p obj) t)))
    (t
     (error "unimpl")))

(defmethod flush-dag ((obj commit-graph))
  "This code is no longer being called for most dags and could probably be removed"
  (when (transient-dag-p)
    (warn "Expected this code to not be running in prod.")
    (bt:with-recursive-lock-held ((lock obj))
      (with-tracing (:flush-dag :id (store-object-id obj))
        (let ((dag (%commit-graph-dag obj)))
          (with-open-file (s (blob-pathname obj) :if-does-not-exist :create
                                                 :direction :output
                                                 :if-exists :supersede)
            (dag:write-to-stream dag s)
            (finish-output s)
            (log:info "Updated commit graph in ~s" (blob-pathname obj)))))))
  (setf (needs-flush-p obj) nil))

(defun flush-dags ()
  (bt:with-lock-held (*flush-lock*)
    (loop for commit-graph in (bknr.datastore:class-instances 'commit-graph)
          if (needs-flush-p commit-graph)
            do (flush-dag commit-graph))))

(def-cron flush-dags (:step-min 30)
  (flush-dags))

(def-store-migration ("Add normalized repos" :version 9)
  (loop for cg in (bknr.datastore:class-instances 'commit-graph)
        if (repo-url cg)
          do (setf (normalized-repo-url cg) (normalize-url (repo-url cg)))))

(deftransaction tx-add-commits (commit-graph commits)
  (let ((dag (commit-graph-persisted-dag commit-graph)))
    (loop for commit in commits
          do
             (dag:add-commit dag commit))))

(defmethod merge-dag-into-commit-graph (commit-graph new-dag)
  (bt:with-recursive-lock-held ((lock commit-graph))
    (when (transient-dag-p)
      (let ((dag (commit-graph-dag commit-graph)))
        (dag:merge-dag dag new-dag)
        (setf (commit-graph-dag commit-graph)
              dag)))
    (let ((dag (commit-graph-persisted-dag commit-graph)))
      (let ((difference (dag:all-commits
                         (dag:dag-difference new-dag dag))))
        (with-batches (commits difference :batch-size 100)
          (tx-add-commits commit-graph commits))))))

(def-store-migration ("Add dag-v2 slot" :version 31)
  (ensure-slot-boundp 'commit-graph 'dag-v2))

(def-store-migration ("Import dag to dag-v2" :version 32)
  (loop for cg in (class-instances 'commit-graph)
        do (merge-dag-into-commit-graph cg (commit-graph-dag cg))))

(deftransaction tx-commit-graph-set-ref (commit-graph name sha)
  (setf
   (commit-graph-refs commit-graph)
   (fset:with
    (commit-graph-refs commit-graph)
    name sha)))

(defmethod commit-graph-set-ref ((commit-graph commit-graph)
                                 (name string)
                                 (sha string))
  (unless (equal sha (fset:lookup (commit-graph-refs commit-graph) name))
    (tx-commit-graph-set-ref commit-graph name sha)))
