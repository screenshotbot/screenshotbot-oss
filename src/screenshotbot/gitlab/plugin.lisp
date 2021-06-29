;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(pkg:define-package :screenshotbot/gitlab/plugin
    (:use #:cl
          #:alexandria)
  (:import-from #:screenshotbot/plugin
                #:plugin
                #:plugin-parse-repo)
  (:import-from #:./repo
                #:gitlab-repo)
  (:import-from #:screenshotbot/model/channel
                #:github-repo)
  (:export #:gitlab-plugin
           #:gitlab-access-token))

(defclass gitlab-plugin (plugin)
  ((access-token :initarg :access-token
                 :accessor gitlab-access-token)))

(defmethod plugin-parse-repo ((plugin gitlab-plugin) company repo-str)
  (declare (ignore company plugin))
  (when (str:containsp "gitlab" repo-str)
    (make-instance 'gitlab-repo
                    :link repo-str
                    :company company
                    :access-token (gitlab-access-token plugin))))
