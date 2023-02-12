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
  (:import-from #:screenshotbot/phabricator/plugin
                #:phab-instance-for-company
                #:phabricator-git-repo
                #:phabricator-plugin)
  (:import-from #:util/phabricator/conduit
                #:make-phab-instance-from-arcrc)
  (:import-from #:screenshotbot/abstract-pr-promoter
                #:check-title
                #:make-promoter-for-acceptable
                #:details-url
                #:check-status
                #:push-remote-check
                #:plugin-installed?
                #:make-acceptable
                #:abstract-pr-acceptable
                #:valid-repo?
                #:abstract-pr-promoter)
  (:import-from #:bknr.datastore
                #:persistent-class)
  (:import-from #:screenshotbot/phabricator/builds
                #:update-diff-status
                #:target-phid
                #:build-phid
                #:find-build-info)
  (:import-from #:screenshotbot/model/recorder-run
                #:phabricator-diff-id
                #:recorder-run-company)
  (:import-from #:screenshotbot/user-api
                #:recorder-run-channel
                #:channel-name)
  (:import-from #:screenshotbot/model/report
                #:report-channel)
  (:import-from #:screenshotbot/report-api
                #:report)
  (:export #:phabricator-promoter))
(in-package :screenshotbot/phabricator/diff-promoter)


(defmethod diff-to-revision ((phab phab-instance) diff-id)
  (parse-integer
   (assoc-value
    (cdar
     (cdar
      (call-conduit phab "differential.querydiffs"
                    `(("ids" . ,(vector diff-id))))))
    :revision-+id+)))

(defmethod create-comment ((phab phab-instance) revision-id message)
  (call-conduit phab "differential.createcomment"
                `(("revision_id" . ,revision-id)
                  ("message" . ,message))))

#+nil
(diff-to-revision (make-phab-instance-from-arcrc "https://phabricator.tdrhq.com/") 7821)
#+nil
(create-comment *phab* 3498 "hello world")

(defclass phabricator-promoter (abstract-pr-promoter)
  ((phab :accessor phab)
   (diff-id :accessor diff-id)))

(defclass diff-acceptable (abstract-pr-acceptable)
  ()
  (:metaclass persistent-class))

(defmethod maybe-promote ((promoter phabricator-promoter) run)
  (call-next-method))

(defmethod valid-repo? ((promoter phabricator-promoter) (repo phabricator-git-repo))
  t)

(defmethod valid-repo? ((promoter phabricator-promoter) repo)
  nil)

(defmethod make-acceptable ((promoter phabricator-promoter) report)
  (make-instance 'diff-acceptable
                 :report report))

(defmethod plugin-installed? ((promoter phabricator-promoter) company repo-url)
  (let ((ret (phabricator-config-for-company company)))
    ret))

(defmethod push-remote-check ((promoter phabricator-promoter) run check)
  (update-diff-status
   (recorder-run-company run)
   (phabricator-diff-id run)
   (ecase (check-status check)
     (:accepted "pass")
     (:rejected "fail")
     (:success "pass")
     (:failed "fail")
     (:action_required "fail")
     (:action-required "fail"))
   :name (channel-name (recorder-run-channel run))
   :details
   (format nil "~a~%~a"
           (check-title check)
           (details-url check))))

(defmethod make-promoter-for-acceptable ((self diff-acceptable))
  (make-instance 'phabricator-promoter))

(defmethod maybe-send-tasks ((promoter phabricator-promoter) run)
  (values))

(defmethod add-comment ((promoter phabricator-promoter) comment)
  (let ((phab (phab promoter)))
    (when (and (url phab)
               (api-key phab))
     (let ((revision (diff-to-revision phab (diff-id promoter))))
       (create-comment phab revision comment)))))

(unregister-promoter 'phabricator-promoter)

(defmethod plugin-promoter ((plugin phabricator-plugin))
  (make-instance 'phabricator-promoter))
