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

(defun can-edit-repo (access-token repo
                      &key user company)
  (let* ((access-token (access-token-str access-token))
         (handle (whoami access-token)))

    (with-audit-log (log (make-instance 'check-collaborator :login handle
                                                            :user user
                                                            :company company
                                                            :repo repo))
      (declare (ignore log))
      (multiple-value-bind (response ret)
          (github-api-request (format nil "/repos/~a/collaborators/~a"
                                      (get-repo-id repo)
                                      handle)
                              :access-token access-token)
        (unless (= ret 204)
          (warn "not a collaborator: ~a" response))
        (values
         (= ret 204)
         (a:assoc-value response :message))))))
