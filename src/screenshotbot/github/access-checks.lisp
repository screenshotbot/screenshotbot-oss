;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(uiop:define-package :screenshotbot/github/access-checks
  (:use #:cl
        #:alexandria)
  (:import-from #:util/java
                #:java-syntax
                #:read-java-field
                #:java-list->list
                #:new-instance)
  (:import-from #:bknr.datastore
                #:persistent-class
                #:store-object
                #:unique-index)
  (:import-from #:screenshotbot/secret
                #:defsecret
                #:secret)
  (:import-from #:screenshotbot/model/recorder-run
                #:github-repo)
  (:import-from #:screenshotbot/git-repo
                #:public-repo-p
                #:get-parent-commit
                #:repo-link
                #:commit-link
                #:generic-git-repo)
  (:import-from #:oidc/oidc
                #:oauth-access-token)
  (:import-from #:screenshotbot/events
                #:push-event)
  (:import-from #:screenshotbot/model/channel
                #:github-get-canonical-repo)
  (:export
   #:github-repo
   #:github-repos-for-user
   #:github-client
   #:get-repo-id
   #:with-throttler
   #:throttler
   #:with-cache)
  (:export #:fix-github-link
           #:github-api-request))
(in-package :screenshotbot/github/access-checks)

(import 'repo-name)

(named-readtables:in-readtable java-syntax)

;; TODO: security

(defclass throttler ()
  ((lock :initform (bt:make-lock)
         :reader throttler-lock)
   (last-ts :initform (local-time:now)
            :accessor last-ts)))

(defparameter *github-throttler* (make-instance 'throttler))

(defun %with-throttler (throttler fn)
  (bt:with-lock-held ((throttler-lock throttler))
    (loop while (local-time:timestamp> (last-ts throttler)
                                       (local-time:timestamp- (local-time:now)
                                                              (* 800 1000 1000)
                                                              :nsec))
          do (progn
               (log:debug "Waiting for throttling lock")
               (sleep 0.2)))
    (setf (last-ts throttler) (local-time:now)))
  (funcall fn))

(defmacro with-throttler ((throttler) &body body)
  `(flet ((body () ,@body))
     (%with-throttler ,throttler #'body)))


(defclass github-repo (generic-git-repo)
  ((cache :initform (make-hash-table :test 'equal)
          :accessor github-repo-cache)
   (commit-cache :initform (make-hash-table :test 'equal)
                 :accessor commit-cache)))

(defsecret :github-user
  "Github user used for accessing the GitHub API. Practically, the
  only time this is used is to check if a repository is public. We
  also use it for some internal code at Screenshotbot.io.

  If you don't care about the public repo logic, you can safely ignore
  this. Leaving this out will treat all GitHub repos as private.")

(defsecret :github-api-secret
  "Github API secret key corresponding to :github-user")


(defun github-api-request (url &key access-token
                                 installation-token)
  (when (typep access-token 'oauth-access-token)
    (setf access-token (oidc/oidc:access-token-str access-token)))
  (check-type access-token (or null string))
  (assert (or access-token installation-token))
  (multiple-value-bind (response code)
      (util/request:http-request
       (format nil "https://api.github.com~a" url)
       :want-string t
       :additional-headers `(("Accept" . "application/vnd.github+json")
                             ("Authorization" .
                                              ,(cond
                                                 (installation-token
                                                  (format nil "token ~a" installation-token))
                                                 (t
                                                  (format nil "Bearer ~a" access-token))))
                             ("X-GitHub-Api-Version" . "2022-11-28")))
    (values
     (cond
       ((str:emptyp response)
        ;; Some api requests don't return a body, we just use the
        ;; response code.
        nil)
       (t
        (json:decode-json-from-string
         response)))
     code)))

(defun github-client (&key
                        oauth-token
                        installation-id)
  (declare (ignore repo))
  (push-event :github.github-client-requested)
  (let ((client (new-instance #,org.eclipse.egit.github.core.client.GitHubClient
                              ))
        (token (or oauth-token installation-id)))
    (cond
      (token
       (#_setOAuth2Token client token))
      (t
       (#_setCredentials client (secret :github-user)
                         (secret :github-api-secret))))
    client))

(defmacro with-cache ((place args) &body body)
  `(flet ((body () ,@body))
     (let ((args ,args)
           (place ,place))
      (symbol-macrolet ((hash-val (gethash  args place)))
        (or
         hash-val
         (setf hash-val (body)))))))

(defun github-repos-for-user (user)
  (let ((res (github-api-request
              (format nil "/users/~a/repos" user)
              :access-token (secret :github-api-secret))))
    (loop for repo in res
          collect (assoc-value repo :html--url))))


(defun get-repo-stars (org repo)
  (with-throttler (*github-throttler*)
    (let ((res (github-api-request
                (format nil "/repos/~a/~a" org repo)
                :access-token (secret :github-api-secret))))
      (values
       (alexandria:assoc-value res :stargazers--count)
       (alexandria:assoc-value res :forks--count)))))

(defun remove-trailing-/ (url)
  (cl-ppcre:regex-replace-all
   "/$" url ""))

(defun repo-string-identifier (repo-url)
  (destructuring-bind (prefix org name)
      (str:rsplit "/" (str:replace-all ":" "/"
                                       (cl-ppcre:regex-replace-all
                                        "[.]git$"
                                        (remove-trailing-/
                                         repo-url) ""))
                  :limit 3)
    (format nil "~a/~a" org name)))


(defvar *public-repo-p-cache* (make-hash-table :test #'equal))

(defmethod public-repo-p ((repo github-repo))
  (when (secret :github-api-secret)
    ;; We use the fake list to make it easier to work with the NIL
    ;; case.
    (first
     (util:or-setf
      (gethash (repo-link repo) *public-repo-p-cache*)
      (let ((repo-id (get-repo-id (repo-link repo))))
        (list
         (not (null (apply #'get-repo-stars
                           (str:split "/" repo-id))))))))))

(defun github-integration-test ()
  (let ((repo (make-instance 'github-repo :link "https://github.com/tdrhq/screenshotbot-example")))
    (assert
     (public-repo-p repo))
    (assert (not (public-repo-p (make-instance 'github-repo :link "https://github.com/tdrhq/web"))))))

;; (github-integration-test)

(defun get-repo-id (repo)
  "An alias for repo-string-identifier"
  (repo-string-identifier repo))


(defmethod commit-link ((repo github-repo) hash)
  (format nil "https://github.com/~a/commit/~a"
          (get-repo-id (repo-link repo))
          hash))
