;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(uiop:define-package :screenshotbot/github/pr-checks
  (:use #:cl #:alexandria)
  (:import-from #:util/java
                #:java-syntax
                #:read-java-field
                #:new-instance)
  (:import-from #:screenshotbot/server
                #:*root*)
  (:import-from #:screenshotbot/github/access-checks
                #:with-throttler
                #:*github-throttler*)
  (:import-from #:screenshotbot/github/jwt-token
                #:github-create-jwt-token
                #:github-request)
  (:import-from #:screenshotbot/github/app-installation
                #:github-get-access-token-for-installation)
  (:import-from #:screenshotbot/events
                #:with-event)
  (:export
   #:github-service
   #:github-update-pull-request))
(in-package :screenshotbot/github/pr-checks)


(named-readtables:in-readtable java-syntax)

(defclass github-service ()
  ((client :initarg github-client)))

(defclass github-checks-service (github-service)
  ())


(defun github-get-credentials (client)
  (read-java-field client "credentials"))


;; (github-get-access-token-for-installation 16121814)

(defun github-create-check-run (full-name
                                &key name
                                  installation-token
                                  head-sha
                                  details-url
                                  status
                                  output
                                  conclusion
                                  external-id)
  (with-throttler (*github-throttler*)
    (let ((response
           (github-request
            (format nil "/repos/~a/check-runs" full-name)
            :method :post
            :json-parameters t
            :installation-token installation-token
            :parameters `(("name" . ,name)
                          ("head_sha" . ,head-sha)
                          ("external_id" . ,(or external-id "internal"))
                          ("conclusion" . ,(or conclusion ""))
                          ,@ (if details-url
                                 `(("details_url" . ,details-url)))
                          ("status" . ,(or status ""))
                          ("output" . ,output)))))
      (log:info "Got response: ~S" response)
      response)))


(defun github-update-pull-request (&rest all-args &key
                                                    app-id
                                                    private-key

                                                    full-name
                                                    check-name
                                                    status
                                                    (installation-id (error "Missing :installation-id"))
                                                    details-url
                                                    output
                                                    conclusion
                                                    head-sha)
  (assert (member status (list nil :queued :in-progress :completed)))
  (log:debug "Updating pull request on ~s" full-name)
  (with-event (:github.create-check-run)
    (github-create-check-run
     full-name
     :name check-name
     :head-sha head-sha
     :conclusion conclusion
     :details-url details-url
     :output output
     :status (when status (str:replace-all "-" "_" (str:downcase status)))
     :installation-token
     (github-get-access-token-for-installation
      installation-id
      :app-id app-id
      :private-key private-key))))
