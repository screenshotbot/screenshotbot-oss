;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/phabricator/maniphest
  (:use #:cl)
  (:import-from #:util/phabricator/conduit
                #:phab-test
                #:whoami
                #:phab-instance
                #:call-conduit)
  (:import-from #:util/phabricator/project
                #:project-phid)
  (:import-from #:alexandria
                #:assoc-value)
  (:export #:create-task))
(in-package :util/phabricator/maniphest)

(defmethod create-task ((phab phab-instance)
                        &key title description project)
  "Create a Maniphest task owned by whoever the API token belongs to.

PROJECT is a project name, or NIL to file the task under no project at
all -- Phabricator is happy with a task that only has an owner."
  (let* ((params `(("title" . ,title)
                   ("description" . ,description)
                   ("ownerPHID" . ,(whoami phab))
                   ,@(when project
                       (list (cons "projectPHIDs"
                                   (vector (project-phid phab project)))))))
         (result (call-conduit phab "maniphest.createtask" params)))
    (assoc-value (assoc-value result :result) :id)))

;; (create-task (phab-test) :title "foobar" :description "stuff here" :project "screenshotbot")


