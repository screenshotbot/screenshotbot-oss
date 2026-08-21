;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;;;;
;;;; The parsing half of the Differential client, against rows shaped the
;;;; way differential.revision.search really returns them -- cl-json's
;;;; identifier conversion and all, which is where the surprises are.

(defpackage :util/phabricator/test-differential
  (:use #:cl
        #:fiveam)
  (:import-from #:util/phabricator/differential
                #:author-phids
                #:make-revision
                #:parse-revision
                #:revision-author
                #:revision-closed-p
                #:revision-date-modified
                #:revision-id
                #:revision-status
                #:revision-title
                #:revision-uri))
(in-package :util/phabricator/test-differential)

(util/fiveam:def-suite)

(defun row (&key (id 13454) (author "PHID-USER-arnold") (status "Draft") closed)
  "A row of differential.revision.search's data, keyed the way cl-json
leaves it: authorPHID arrives as :AUTHOR-+PHID+ and dateModified as
:DATE-MODIFIED."
  `((:id . ,id)
    (:type . "DREV")
    (:phid . "PHID-DREV-764z6dm7ftxzt42cjn6w")
    (:fields
     (:title . "\"Create Task\" for the LispWorks IDE")
     (:uri . "https://phabricator.tdrhq.com/D13454")
     (:author-+phid+ . ,author)
     (:status (:value . "draft") (:name . ,status) (:closed . ,closed))
     (:date-created . 1787331507)
     (:date-modified . 1787331507)
     (:summary . "A subject, a body, and a Maniphest task."))
    (:attachments)))

(defun names (&rest pairs)
  (let ((table (make-hash-table :test #'equal)))
    (loop for (phid name) on pairs by #'cddr
          do (setf (gethash phid table) name))
    table))

(test a-row-becomes-a-revision
  (let ((revision (parse-revision (row) (names "PHID-USER-arnold" "arnold"))))
    (is (eql 13454 (revision-id revision)))
    (is (equal "\"Create Task\" for the LispWorks IDE" (revision-title revision)))
    (is (equal "https://phabricator.tdrhq.com/D13454" (revision-uri revision)))
    (is (equal "Draft" (revision-status revision)))
    (is (equal "arnold" (revision-author revision)))
    (is (eql 1787331507 (revision-date-modified revision)))
    (is-false (revision-closed-p revision))))

(test an-author-we-couldnt-name-keeps-their-phid
  (let ((revision (parse-revision (row) (names))))
    (is (equal "PHID-USER-arnold" (revision-author revision)))))

(test closed-is-a-boolean-whatever-phabricator-sent
  ;; The field is absent for open revisions and a string for closed ones.
  (is-false (revision-closed-p (parse-revision (row :closed nil) (names))))
  (is-true (revision-closed-p
            (parse-revision (row :closed "closed" :status "Closed") (names)))))

(test authors-are-looked-up-once-each
  (let ((rows (list (row :id 1 :author "PHID-USER-a")
                    (row :id 2 :author "PHID-USER-b")
                    (row :id 3 :author "PHID-USER-a"))))
    (is (equal '("PHID-USER-a" "PHID-USER-b")
               (sort (copy-list (author-phids rows)) #'string<))))
  ;; A row with no author at all doesn't put NIL in the query.
  (is (equal '() (author-phids (list (row :author nil))))))
