;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/github/read-repos
  (:use #:cl)
  (:import-from #:screenshotbot/github/access-checks
                #:github-api-request
                #:get-repo-id
                #:github-user-service
                #:github-repo-id
                #:github-client)
  (:import-from #:oidc/oidc
                #:access-token-str)
  (:import-from #:screenshotbot/audit-log
                #:with-audit-log)
  (:import-from #:screenshotbot/github/audit-log
                #:check-collaborator)
  (:import-from #:screenshotbot/user-api
                #:current-company)
  (:import-from #:util/misc
                #:not-null!)
  (:import-from #:screenshotbot/github/jwt-token
                #:github-request)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:read-repo-list
   #:can-edit-repo))
(in-package :screenshotbot/github/read-repos)

(defun whoami (access-token)
  (let ((response (github-api-request "/user"
                                      :access-token access-token)))
    (not-null!
     (a:assoc-value
      response
      :login))))

(defun repo-collaborator-p (repo handle &key access-token
                                          installation-token
                                          company)
  (log:info "Checking for repo collaboractor: ~a, ~a" repo handle)
  (with-audit-log (log (make-instance 'check-collaborator :login handle
                                                          :company company
                                                          :repo repo))
    (declare (ignore log))
    (let ((url (format nil "/repos/~a/collaborators/~a"
                                    (get-repo-id repo)
                                    handle)))
      (log:info "url is: ~a " url)
      (multiple-value-bind (response ret)
          (github-api-request url
                              :access-token access-token
                              :installation-token installation-token)
        (unless (= ret 204)
          (warn "not a collaborator: ~a" response))
        (values
         (= ret 204)
         (a:assoc-value response :message))))))

(defun can-edit-repo (access-token repo
                      &key user company)
  (let* ((access-token (cond
                         ((stringp access-token)
                          access-token)
                         (t
                          (access-token-str access-token))))
         (handle (whoami access-token)))
    (repo-collaborator-p repo handle
                         :access-token access-token
                         :company company)))
