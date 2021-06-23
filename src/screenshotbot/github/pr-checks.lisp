;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(pkg:define-package :screenshotbot/github/pr-checks
    (:use #:cl
          #:alexandria)
  (:import-from #:../java
                #:java-syntax
                #:read-java-field
                #:new-instance)
  (:import-from #:../server
                #:*root*)
  (:import-from #:./access-checks
                #:with-throttler
                #:*github-throttler*)
  (:import-from #:./jwt-token
                #:github-create-jwt-token
                #:github-request)
  (:export #:github-service
           #:github-update-pull-request))


(named-readtables:in-readtable java-syntax)

(defclass github-service ()
  ((client :initarg github-client)))

(defclass github-checks-service (github-service)
  ())


(defun github-get-credentials (client)
  (read-java-field client "credentials"))


(defun github-get-access-token-for-installation (installation-id &key
                                                                   app-id
                                                                   private-key)
  (with-throttler (*github-throttler*)
   (assoc-value (github-request
                 (format nil "/app/installations/~a/access_tokens" installation-id)
                 :jwt-token (github-create-jwt-token
                             :app-id app-id
                             :private-key private-key)
                 :method :post)
                :token)))

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
  (log:info "Updating pull request ~s" all-args)
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
    :private-key private-key)))
