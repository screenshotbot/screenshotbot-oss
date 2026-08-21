;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/phabricator/differential
  (:use #:cl)
  (:import-from #:util/phabricator/conduit
                #:call-conduit
                #:search-conduit
                #:phab-instance)
  (:import-from #:util/phabricator/harbormaster
                #:builds-for-diffs)
  (:import-from #:alexandria
                #:assoc-value)
  (:export #:search-revisions
           #:revision
           #:revision-p
           #:revision-id
           #:revision-phid
           #:revision-diff-phid
           #:revision-title
           #:revision-uri
           #:revision-status
           #:revision-closed-p
           #:revision-author
           #:revision-date-modified
           #:revision-summary
           #:revision-builds
           #:attach-builds))
(in-package :util/phabricator/differential)

;;; Reading Differential: the revisions on an install, through
;;; differential.revision.search.
;;;
;;; The search returns author PHIDs rather than names, so a second call to
;;; phid.query turns those into something readable. That one is per page
;;; of results, not per revision, and the same goes for the builds.

(defstruct revision
  id phid diff-phid title uri status closed-p author date-modified summary
  builds)

;; * Reading the results

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
     ;; The diff the revision currently points at. Harbormaster hangs its
     ;; buildables off this, not off the revision.
     :diff-phid (assoc-value fields :diff-+phid+)
     :title (assoc-value fields :title)
     :uri (assoc-value fields :uri)
     :status (assoc-value status :name)
     :closed-p (and (assoc-value status :closed) t)
     :author (or (gethash phid names) phid)
     :date-modified (assoc-value fields :date-modified)
     :summary (assoc-value fields :summary))))

;; * The search itself

(defmethod attach-builds ((phab phab-instance) revisions)
  "Fill in each revision's BUILDS from Harbormaster, and return them.

Two calls for the whole list rather than two per revision, which is why
this takes all of them at once."
  (let ((builds (builds-for-diffs phab (remove nil (mapcar #'revision-diff-phid
                                                           revisions)))))
    (dolist (revision revisions revisions)
      (setf (revision-builds revision)
            (gethash (revision-phid revision) builds)))))

(defmethod search-revisions ((phab phab-instance) &key (limit 100) constraints
                                                       with-builds)
  "The revisions on PHAB, newest first, at most LIMIT of them.

CONSTRAINTS is an alist passed through to differential.revision.search,
or NIL for every revision on the install. WITH-BUILDS fills in what
Harbormaster ran for each of them, which is two more calls for the whole
list.

Returns the revisions, and as a second value whether Phabricator had
more to give. An install has far more revisions than anyone wants in a
list, so a caller that stops early is expected to say so rather than
present a prefix as the whole thing."
  (multiple-value-bind (rows morep)
      (search-conduit phab "differential.revision.search"
                      :constraints constraints
                      :limit limit)
    (let* ((names (author-names phab (author-phids rows)))
           (revisions (loop for row in rows collect (parse-revision row names))))
      (when with-builds
        (attach-builds phab revisions))
      (values revisions morep))))
