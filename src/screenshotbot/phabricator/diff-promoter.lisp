;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(uiop:define-package :screenshotbot/phabricator/diff-promoter
  (:use #:cl
        #:alexandria
        #:util/phabricator/conduit
        #:screenshotbot/model/company
        #:screenshotbot/model/recorder-run
        #:screenshotbot/promote-api
        #:screenshotbot/phabricator/commenting-promoter)
  (:export #:phabricator-promoter))
(in-package :screenshotbot/phabricator/diff-promoter)

(defmethod diff-to-revision ((phab phab-instance) diff-id)
  (parse-integer
   (assoc-value
    (cdar
     (cdar
      (call-conduit phab "differential.querydiffs"
                    `(("ids[0]" . ,diff-id)))))
    :revision-+id+)))

(defmethod create-comment ((phab phab-instance) revision-id message)
  (call-conduit phab "differential.createcomment"
                `(("revision_id" . ,revision-id)
                  ("message" . ,message))))

#+nil
(diff-to-revision *phab* 7821)
#+nil
(create-comment *phab* 3498 "hello world")

(defclass phabricator-promoter (commenting-promoter)
  ((phab :accessor phab)
   (diff-id :accessor diff-id)))

(defmethod maybe-promote ((promoter phabricator-promoter) run)
  (when (phabricator-diff-id run)
    (call-next-method)))

(defmethod maybe-send-tasks ((promoter phabricator-promoter) run)
  (when (phabricator-diff-id run)
    (let ((config (phabricator-config-for-company (recorder-run-company run))))
      (setf (phab promoter) (make-instance 'phab-instance
                                            :url (phabricator-url config)
                                            :api-key (conduit-api-key config)))
      (setf (diff-id promoter) (phabricator-diff-id run)))
    (call-next-method)))

(defmethod add-comment ((promoter phabricator-promoter) comment)
  (let ((phab (phab promoter)))
    (when (and (url phab)
               (api-key phab))
     (let ((revision (diff-to-revision phab (diff-id promoter))))
       (create-comment phab revision comment)))))

(register-promoter 'phabricator-promoter)
