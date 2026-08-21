;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/phabricator/differential
  (:use #:cl)
  (:import-from #:util/phabricator/conduit
                #:call-conduit
                #:phab-instance)
  (:import-from #:alexandria
                #:alist-hash-table
                #:assoc-value)
  (:export #:search-revisions
           #:revision
           #:revision-p
           #:revision-id
           #:revision-phid
           #:revision-title
           #:revision-uri
           #:revision-status
           #:revision-closed-p
           #:revision-author
           #:revision-date-modified
           #:revision-summary
           #:*page-size*))
(in-package :util/phabricator/differential)

;;; Reading Differential: the revisions on an install, through
;;; differential.revision.search.
;;;
;;; The search returns author PHIDs rather than names, so a second call to
;;; phid.query turns those into something readable. That one is per page
;;; of results, not per revision.

(defstruct revision
  id phid title uri status closed-p author date-modified summary)

(defparameter *page-size* 100
  "How many revisions to ask for in one call. Phabricator's own maximum
is 100, so asking for more gets you 100 anyway.")

;; * Reading a page of results

(defun revision-page (phab after constraints limit)
  "One call to differential.revision.search, newest revision first.

Returns the raw rows, and the cursor to continue from as a second value
-- NIL when that was the last page."
  (let* ((params `(("limit" . ,(min limit *page-size*))
                   ,@(when after
                       (list (cons "after" after)))
                   ,@(when constraints
                       (list (cons "constraints"
                                   (alist-hash-table constraints :test #'equal))))))
         (result (assoc-value (call-conduit phab "differential.revision.search" params)
                              :result)))
    (values (assoc-value result :data)
            (assoc-value (assoc-value result :cursor) :after))))

(defun author-phids (rows)
  "The distinct author PHIDs in ROWS, in one list to look up together."
  (remove-duplicates
   (loop for row in rows
         for phid = (assoc-value (assoc-value row :fields) :author-+phid+)
         when phid
           collect phid)
   :test #'equal))

(defun author-names (phab phids)
  "PHIDs to usernames, in a single phid.query.

The result is keyed by PHID, and cl-json's identifier conversion doesn't
leave a PHID in any state to look up by, so the names come out of the
values instead, where the PHID is a field like any other."
  (let ((names (make-hash-table :test #'equal)))
    (when phids
      (let ((result (assoc-value
                     (call-conduit phab "phid.query"
                                   `(("phids" . ,(coerce phids 'vector))))
                     :result)))
        (loop for entry in result
              for object = (cdr entry)
              for phid = (assoc-value object :phid)
              for name = (assoc-value object :name)
              when (and phid name)
                do (setf (gethash phid names) name))))
    names))

(defun parse-revision (row names)
  "One row of differential.revision.search as a REVISION.

NAMES is the table from AUTHOR-NAMES. An author who isn't in it keeps
their PHID, which is at least a thing you can search for."
  (let* ((fields (assoc-value row :fields))
         (status (assoc-value fields :status))
         (phid (assoc-value fields :author-+phid+)))
    (make-revision
     :id (assoc-value row :id)
     :phid (assoc-value row :phid)
     :title (assoc-value fields :title)
     :uri (assoc-value fields :uri)
     :status (assoc-value status :name)
     :closed-p (and (assoc-value status :closed) t)
     :author (or (gethash phid names) phid)
     :date-modified (assoc-value fields :date-modified)
     :summary (assoc-value fields :summary))))

;; * The search itself

(defmethod search-revisions ((phab phab-instance) &key (limit 100) constraints)
  "The revisions on PHAB, newest first, at most LIMIT of them.

CONSTRAINTS is an alist passed through to differential.revision.search,
or NIL for every revision on the install.

Returns the revisions, and as a second value whether Phabricator had
more to give. An install has far more revisions than anyone wants in a
list, so a caller that stops early is expected to say so rather than
present a prefix as the whole thing."
  (let ((rows '())
        (after nil)
        (morep nil))
    (loop
      (multiple-value-bind (page next)
          (revision-page phab after constraints (- limit (length rows)))
        (setf rows (append rows page)
              after next)
        (when (or (null page) (null next) (>= (length rows) limit))
          (setf morep (and next (>= (length rows) limit) t))
          (return))))
    (when (> (length rows) limit)
      (setf rows (subseq rows 0 limit)))
    (values (let ((names (author-names phab (author-phids rows))))
              (loop for row in rows collect (parse-revision row names)))
            morep)))
