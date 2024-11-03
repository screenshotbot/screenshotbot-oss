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
                #:assoc-value))
(in-package :util/phabricator/maniphest)

(defmethod create-task ((phab phab-instance)
                        &key title description project)
  (let ((result (call-conduit
                 phab
                 "maniphest.createtask"
                 `(("title" . ,title)
                   ("description" . ,description)
                   ("ownerPHID" . ,(whoami phab))
                   ("projectPHIDs" . #(,(project-phid phab project)))))))
    (assoc-value (assoc-value result :result) :id)))

;; (create-task (phab-test) :title "foobar" :description "stuff here" :project "screenshotbot")


