;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/gitlab/webhook
  (:use #:cl)
  (:import-from #:screenshotbot/webhook/model
                #:webhook-payload)
  (:import-from #:util/json-mop
                #:ext-json-serializable-class))
(in-package :screenshotbot/gitlab/webhook)

(defclass gitlab-update-build-status-payload (webhook-payload)
  ((repo-id :initarg :project-path
            :json-key "repoId"
            :json-type :string
            :documentation "The repository ID. e.g. myorg/myproject. Typically, you'll url-encode this when passing it to the GitLab API. e.g. /projects/myorg%2Fmyproject/statuses/abcd0000.")
   (sha :initarg :sha
        :json-key "sha"
        :json-type :string
        :documentation "The commit SHA being updated")
   (state :initarg :state
          :json-key "state"
          :json-type :string
          :documentation "The state of the build status, one of: success, failed orpending")
   (target-url :initarg :target-url
               :json-key "targetUrl"
               :json-type :string
               :documentation "The URL to be linked to, typically a link to Screenshotbot ")
   (name :initarg :name
         :json-key "name"
         :json-type :string)
   (description :initarg :description
                :json-key "description"
                :json-type :string)
   (company :initarg :company
            :documentation "Ignored in the JSON payload"))
  (:metaclass ext-json-serializable-class)
  (:default-initargs :event "gitlab.update-build-status")
  (:documentation "If webhooks are enabled for GitLab integration, this webhook is
dispatched each time we update your GitLab build status. This webhook
is also dispatched if you don't provide us a GitLab access token, so
it can be used to set up an integration with GitLab in a manner that
restricts Screenshotbot's access to your GitLab."))
