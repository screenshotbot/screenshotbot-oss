;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(uiop:define-package :screenshotbot/model/commit-graph
  (:use #:cl #:alexandria)
  (:import-from #:bknr.datastore
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
  (:export
   #:commit-graph
   #:repo-url
   #:lock
   #:find-commit-graph
   #:find-or-create-commit-graph
   #:commit-graph-dag))
(in-package :screenshotbot/model/commit-graph)

(defvar *flush-lock* (bt:make-lock "dag-flush-lock"))

(with-class-validation
  (defclass commit-graph (bknr.datastore:blob)
    ((url :type string
          :initform nil
          :accessor repo-url
          :initarg :url)
     (company :initarg :company
              :accessor company
              :initform nil)
     (lock :initform (bt:make-recursive-lock)
           :transient t
           :accessor lock)
     (dag :initform nil
          :transient t
          :accessor %commit-graph-dag)
     (needs-flush-p :initform nil
                    :transient t
                    :accessor needs-flush-p))
    (:metaclass persistent-class)))

(defmethod find-commit-graph ((company company) (url string))
  (log:info "Finding commit graph for company ~a and repo ~a" company url)
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
                      :company company)))))

(defmethod commit-graph-dag ((obj commit-graph))
  (util:or-setf
   (%commit-graph-dag obj)
   (bt:with-recursive-lock-held ((lock obj))
     (cond
       ((not (path:-e (blob-pathname obj)))
        (make-instance 'dag:dag))
       (t
        (with-open-file (s (blob-pathname obj) :direction :input)
          (dag:read-from-stream s)))))))

(defmethod (setf commit-graph-dag) (dag (obj commit-graph))
  (setf (%commit-graph-dag obj) dag)
  (setf (needs-flush-p obj) t))

(defmethod flush-dag ((obj commit-graph))
  (bt:with-recursive-lock-held ((lock obj))
    (let ((dag (%commit-graph-dag obj)))
     (with-open-file (s (blob-pathname obj) :if-does-not-exist :create
                                            :direction :output
                                            :if-exists :supersede)
       (dag:write-to-stream dag s)
       (finish-output s)
       (log:info "Updated commit graph in ~s" (blob-pathname obj)))))
  (setf (needs-flush-p obj) nil))

(defun flush-dags ()
  (bt:with-lock-held (*flush-lock*)
    (loop for commit-graph in (bknr.datastore:class-instances 'commit-graph)
          if (needs-flush-p commit-graph)
            do (flush-dag commit-graph))))

(defmethod check-integrity ((commit-graph commit-graph))
  (when-let ((dag (%commit-graph-dag commit-graph)))
    (dag:check-integrity dag)))

(defun check-integrity-for-all ()
  (mapc #'check-integrity (bknr.datastore:class-instances 'commit-graph)))

(def-cron flush-dags (:step-min 5)
  (flush-dags))
