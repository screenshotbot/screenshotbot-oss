;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(pkg:define-package :screenshotbot/model/commit-graph
    (:use #:cl
          #:alexandria)
  (:import-from #:bknr.datastore
                #:store-objects-with-class
                #:blob-pathname
                #:persistent-class)
  (:import-from #:../git-repo
                #:commit-graph-dag
                #:find-or-create-commit-graph
                #:commit-graph)
  (:import-from #:./company
                #:company)
  (:export #:commit-graph
           #:repo-url
           #:lock
           #:find-commit-graph
           #:find-or-create-commit-graph
           #:commit-graph-dag))

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
         :accessor lock))
  (:metaclass persistent-class))

(defmethod find-commit-graph ((company company) (url string))
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
  (bt:with-lock-held ((lock obj))
   (cond
     ((not (path:-e (blob-pathname obj)))
      (make-instance 'dag:dag))
     (t
      (with-open-file (s (blob-pathname obj) :direction :input)
        (dag:read-from-stream s))))))

(defmethod (setf commit-graph-dag) (dag (obj commit-graph))
  (bt:with-lock-held ((lock obj))
    (with-open-file (s (blob-pathname obj) :if-does-not-exist :create
                                           :direction :output
                                           :if-exists :supersede)
      (dag:write-to-stream dag s)
      (finish-output s)
      (log:info "Updated commit graph in ~s" (blob-pathname obj)))))
