;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(uiop:define-package :screenshotbot/github/webhook
  (:use #:cl #:alexandria)
  (:import-from #:bknr.datastore
                #:store-object
                #:persistent-class
                #:hash-index
                #:with-transaction)
  (:import-from #:screenshotbot/model
                #:github-get-canonical-repo)
  (:import-from #:screenshotbot/server
                #:defhandler)
  (:import-from #:screenshotbot/model/company
                #:installation-id)
  (:import-from #:screenshotbot/model/recorder-run
                #:github-repo)
  (:import-from #:screenshotbot/github/plugin
                #:webhook-relays
                #:webhook-secret
                #:github-plugin)
  (:import-from #:util/threading
                #:with-extras
                #:make-thread
                #:max-pool
                #:ignore-and-log-errors)
  (:import-from #:util/request
                #:http-request)
  (:export
   #:pull-request
   #:github-get-canonical-repo
   #:repo-full-name
   #:pull-request-id
   #:pull-request-head
   #:pull-request-base
   #:all-pull-requests
   #:pull-request-with-url
   #:github-maybe-update-pull-request
   #:*hooks*))
(in-package :screenshotbot/github/webhook)

(defmethod github-get-canonical-repo (repo)
  (let ((host (if (str:containsp "bitbucket" repo)
                  "bitbucket.org"
                  "github.com")))
   (cl-ppcre:regex-replace-all
    (format nil "^(ssh://)?git@~a[:/]" host)
    (cl-ppcre:regex-replace-all
     "https://api."
     (cl-ppcre:regex-replace-all "[.]git$"
                                 (cl-ppcre:regex-replace-all "^git://"
                                  repo "https://")
                                 "")
     "https://")
    (format nil "https://~a/" host))))

(defclass pull-request (store-object)
  ((url
    :initarg :url
    :index-type hash-index
    :index-initargs (:test #'equal)
    :index-reader pull-requests-with-url
    :index-values all-pull-requests)
   (pull-id
    :initarg :pull-id
    :initform nil
    :accessor pull-request-id)
   (repo-full-name
    :initarg :repo-full-name
    :accessor repo-full-name)
   (head
    :initarg :head
    :accessor pull-request-head)
   (base
    :initarg :base
    :accessor pull-request-base))
  (:metaclass persistent-class))

(defun pull-request-with-url (url)
  (car (pull-requests-with-url url)))

(defun channels-for-pull-request (pull-request))

(defvar *hooks* nil)

(defvar *thread-pool* (make-instance 'max-pool
                                     :max 20))

(defun relay-one-webhook (relay data &key signature)
  (let ((url (quri:render-uri
              (quri:merge-uris
               "/github-webhook"
               (quri:uri relay)))))
    (log:info "Relaying to ~a" url)
    (make-thread
     (lambda ()
       (http-request
        url
        :additional-headers `(("x-hub-signature-256" . ,signature))
        :method :post
        :external-format-out :utf-8
        :content (flexi-streams:octets-to-string data
                                                 :external-format :utf-8)))

     :pool *thread-pool*)))

(defun maybe-relay-webhook (plugin data &key  signature)
  (dolist (relay (webhook-relays plugin))
    (relay-one-webhook relay
                       data :signature signature)))

(defhandler (nil :uri "/github-webhook") ()
  (let* ((plugin (github-plugin))
         (webhook-secret (webhook-secret plugin)))
    (let ((stream (hunchentoot:raw-post-data
                   :want-stream t
                   :force-binary t))
          (length (parse-integer (hunchentoot:header-in* :content-length)))
          (signature (hunchentoot:header-in* :x-hub-signature-256)))
      (let ((data (make-array length :element-type 'flexi-streams:octet )))
        (read-sequence data stream)
        (maybe-relay-webhook
         plugin
         data
         :signature signature)
        (make-thread
         (lambda ()
           (ignore-and-log-errors ()
             (validate-hmac :webhook-secret webhook-secret
                            :data data
                            :signature signature)
             (let ((json (json:decode-json
                          (flexi-streams:make-flexi-stream
                           (flexi-streams:make-in-memory-input-stream data)))))
               (log:debug "got json: ~a" json)
               ;; todo: does this next call actually do anything? We used to
               ;; use it before when we did special code for Pull Requests,
               ;; and eventually just moved to the checks API. I think it
               ;; could go.
               (loop for hook in *hooks*
                     do (funcall hook json))
               (let ((pull-request (github-maybe-update-pull-request json)))
                 (declare (ignore pull-request))))
             ))
         :name "github-webhook"
         :pool *thread-pool*))
      "OK")))

(defun validate-hmac (&key webhook-secret
                        data
                        signature)
  (let ((hmac (ironclad:make-hmac (flexi-streams:string-to-octets
                                   webhook-secret)
                                  :sha256)))
    (ironclad:update-hmac hmac data)
    (let* ((expected (ironclad:hmac-digest hmac))
           (actual signature)
           (expected (format nil "sha256=~a"
                             (ironclad:byte-array-to-hex-string expected))))
      (unless (equal expected actual)
        (with-extras (("data" data)
                      ("decoded-data"(flex:octets-to-string data :external-format :utf-8)))
         (error "invalid hmac, expected ~a, got ~a" expected actual)))))
  (log:debug "hmac validated"))

(defun github-maybe-update-pull-request (json)
  (let ((obj (assoc-value json :pull--request)))
    (when obj
      (let* ((url (assoc-value obj :url))
             (full-name (assoc-value (assoc-value (assoc-value obj :base) :repo)
                                     :full--name))
             (head (assoc-value (assoc-value obj :head) :sha))
             (base (assoc-value (assoc-value obj :base) :sha))
             (pull-id (assoc-value obj :number))
             (pull-request
               (progn
                 (assert url)
                 (make-instance 'pull-request
                                :url url
                                :head head
                                :pull-id pull-id
                                :base base
                                :repo-full-name full-name))))
        pull-request))))

#| Sample webhook

 <INFO> [19:23:15] screenshotbot webhook.lisp () - got json: ((ACTION . requested) (CHECK--SUITE (ID . 1803773884) (NODE--ID . MDEwOkNoZWNrU3VpdGUxODAzNzczODg0) (HEAD--BRANCH . master) (HEAD--SHA . 8a8624afe32055f07568bf865bfe01b56b4a9c0e) (STATUS . queued) (CONCLUSION) (URL . https://api.github.com/repos/tdrhq/fast-example/check-suites/1803773884) (BEFORE . 6cb7b86ba09eac27f9b2eb840dbd83ad85324a44) (AFTER . 8a8624afe32055f07568bf865bfe01b56b4a9c0e) (PULL--REQUESTS) (APP (ID . 84260) (SLUG . screenshotbot) (NODE--ID . MDM6QXBwODQyNjA=) (OWNER (LOGIN . screenshotbot) (ID . 72519534) (NODE--ID . MDQ6VXNlcjcyNTE5NTM0) (AVATAR--URL . https://avatars3.githubusercontent.com/u/72519534?v=4) (GRAVATAR--ID . ) (URL . https://api.github.com/users/screenshotbot) (HTML--URL . https://github.com/screenshotbot) (FOLLOWERS--URL . https://api.github.com/users/screenshotbot/followers) (FOLLOWING--URL . https://api.github.com/users/screenshotbot/following{/other_user}) (GISTS--URL . https://api.github.com/users/screenshotbot/gists{/gist_id}) (STARRED--URL . https://api.github.com/users/screenshotbot/starred{/owner}{/repo}) (SUBSCRIPTIONS--URL . https://api.github.com/users/screenshotbot/subscriptions) (ORGANIZATIONS--URL . https://api.github.com/users/screenshotbot/orgs) (REPOS--URL . https://api.github.com/users/screenshotbot/repos) (EVENTS--URL . https://api.github.com/users/screenshotbot/events{/privacy}) (RECEIVED--EVENTS--URL . https://api.github.com/users/screenshotbot/received_events) (TYPE . User) (SITE--ADMIN)) (NAME . screenshotbot) (DESCRIPTION . Screenshotbot comments on Pull Requests, and opens issues on github repos, when we detect changes in your app's rendering.) (EXTERNAL--URL . https://screenshotbot.io) (HTML--URL . https://github.com/apps/screenshotbot) (CREATED--AT . 2020-10-10T00:21:39Z) (UPDATED--AT . 2020-12-11T21:01:37Z) (PERMISSIONS (CHECKS . write) (CONTENTS . read) (EMAILS . read) (METADATA . read)) (EVENTS)) (CREATED--AT . 2021-01-11T03:23:14Z) (UPDATED--AT . 2021-01-11T03:23:14Z) (LATEST--CHECK--RUNS--COUNT . 0) (CHECK--RUNS--URL . https://api.github.com/repos/tdrhq/fast-example/check-suites/1803773884/check-runs) (HEAD--COMMIT (ID . 8a8624afe32055f07568bf865bfe01b56b4a9c0e) (TREE--ID . 87cdbd23db49c2b39c8571e0deb21ffe36ffece2) (MESSAGE . ..) (TIMESTAMP . 2021-01-11T03:23:09Z) (AUTHOR (NAME . Arnold Noronha) (EMAIL . arnold@tdrhq.com)) (COMMITTER (NAME . Arnold Noronha) (EMAIL . arnold@tdrhq.com)))) (REPOSITORY (ID . 328090591) (NODE--ID . MDEwOlJlcG9zaXRvcnkzMjgwOTA1OTE=) (NAME . fast-example) (FULL--NAME . tdrhq/fast-example) (PRIVATE) (OWNER (LOGIN . tdrhq) (ID . 82582) (NODE--ID . MDQ6VXNlcjgyNTgy) (AVATAR--URL . https://avatars1.githubusercontent.com/u/82582?v=4) (GRAVATAR--ID . ) (URL . https://api.github.com/users/tdrhq) (HTML--URL . https://github.com/tdrhq) (FOLLOWERS--URL . https://api.github.com/users/tdrhq/followers) (FOLLOWING--URL . https://api.github.com/users/tdrhq/following{/other_user}) (GISTS--URL . https://api.github.com/users/tdrhq/gists{/gist_id}) (STARRED--URL . https://api.github.com/users/tdrhq/starred{/owner}{/repo}) (SUBSCRIPTIONS--URL . https://api.github.com/users/tdrhq/subscriptions) (ORGANIZATIONS--URL . https://api.github.com/users/tdrhq/orgs) (REPOS--URL . https://api.github.com/users/tdrhq/repos) (EVENTS--URL . https://api.github.com/users/tdrhq/events{/privacy}) (RECEIVED--EVENTS--URL . https://api.github.com/users/tdrhq/received_events) (TYPE . User) (SITE--ADMIN)) (HTML--URL . https://github.com/tdrhq/fast-example) (DESCRIPTION) (FORK) (URL . https://api.github.com/repos/tdrhq/fast-example) (FORKS--URL . https://api.github.com/repos/tdrhq/fast-example/forks) (KEYS--URL . https://api.github.com/repos/tdrhq/fast-example/keys{/key_id}) (COLLABORATORS--URL . https://api.github.com/repos/tdrhq/fast-example/collaborators{/collaborator}) (TEAMS--URL . https://api.github.com/repos/tdrhq/fast-example/teams) (HOOKS--URL . https://api.github.com/repos/tdrhq/fast-example/hooks) (ISSUE--EVENTS--URL . https://api.github.com/repos/tdrhq/fast-example/issues/events{/number}) (EVENTS--URL . https://api.github.com/repos/tdrhq/fast-example/events) (ASSIGNEES--URL . https://api.github.com/repos/tdrhq/fast-example/assignees{/user}) (BRANCHES--URL . https://api.github.com/repos/tdrhq/fast-example/branches{/branch}) (TAGS--URL . https://api.github.com/repos/tdrhq/fast-example/tags) (BLOBS--URL . https://api.github.com/repos/tdrhq/fast-example/git/blobs{/sha}) (GIT--TAGS--URL . https://api.github.com/repos/tdrhq/fast-example/git/tags{/sha}) (GIT--REFS--URL . https://api.github.com/repos/tdrhq/fast-example/git/refs{/sha}) (TREES--URL . https://api.github.com/repos/tdrhq/fast-example/git/trees{/sha}) (STATUSES--URL . https://api.github.com/repos/tdrhq/fast-example/statuses/{sha}) (LANGUAGES--URL . https://api.github.com/repos/tdrhq/fast-example/languages) (STARGAZERS--URL . https://api.github.com/repos/tdrhq/fast-example/stargazers) (CONTRIBUTORS--URL . https://api.github.com/repos/tdrhq/fast-example/contributors) (SUBSCRIBERS--URL . https://api.github.com/repos/tdrhq/fast-example/subscribers) (SUBSCRIPTION--URL . https://api.github.com/repos/tdrhq/fast-example/subscription) (COMMITS--URL . https://api.github.com/repos/tdrhq/fast-example/commits{/sha}) (GIT--COMMITS--URL . https://api.github.com/repos/tdrhq/fast-example/git/commits{/sha}) (COMMENTS--URL . https://api.github.com/repos/tdrhq/fast-example/comments{/number}) (ISSUE--COMMENT--URL . https://api.github.com/repos/tdrhq/fast-example/issues/comments{/number}) (CONTENTS--URL . https://api.github.com/repos/tdrhq/fast-example/contents/{+path}) (COMPARE--URL . https://api.github.com/repos/tdrhq/fast-example/compare/{base}...{head}) (MERGES--URL . https://api.github.com/repos/tdrhq/fast-example/merges) (ARCHIVE--URL . https://api.github.com/repos/tdrhq/fast-example/{archive_format}{/ref}) (DOWNLOADS--URL . https://api.github.com/repos/tdrhq/fast-example/downloads) (ISSUES--URL . https://api.github.com/repos/tdrhq/fast-example/issues{/number}) (PULLS--URL . https://api.github.com/repos/tdrhq/fast-example/pulls{/number}) (MILESTONES--URL . https://api.github.com/repos/tdrhq/fast-example/milestones{/number}) (NOTIFICATIONS--URL . https://api.github.com/repos/tdrhq/fast-example/notifications{?since,all,participating}) (LABELS--URL . https://api.github.com/repos/tdrhq/fast-example/labels{/name}) (RELEASES--URL . https://api.github.com/repos/tdrhq/fast-example/releases{/id}) (DEPLOYMENTS--URL . https://api.github.com/repos/tdrhq/fast-example/deployments) (CREATED--AT . 2021-01-09T06:28:06Z) (UPDATED--AT . 2021-01-11T03:22:34Z) (PUSHED--AT . 2021-01-11T03:23:14Z) (GIT--URL . git://github.com/tdrhq/fast-example.git) (SSH--URL . git@github.com:tdrhq/fast-example.git) (CLONE--URL . https://github.com/tdrhq/fast-example.git) (SVN--URL . https://github.com/tdrhq/fast-example) (HOMEPAGE) (SIZE . 5) (STARGAZERS--COUNT . 0) (WATCHERS--COUNT . 0) (LANGUAGE . Shell) (HAS--ISSUES . T) (HAS--PROJECTS . T) (HAS--DOWNLOADS . T) (HAS--WIKI . T) (HAS--PAGES) (FORKS--COUNT . 0) (MIRROR--URL) (ARCHIVED) (DISABLED) (OPEN--ISSUES--COUNT . 21) (LICENSE) (FORKS . 0) (OPEN--ISSUES . 21) (WATCHERS . 0) (DEFAULT--BRANCH . master)) (SENDER (LOGIN . tdrhq) (ID . 82582) (NODE--ID . MDQ6VXNlcjgyNTgy) (AVATAR--URL . https://avatars1.githubusercontent.com/u/82582?v=4) (GRAVATAR--ID . ) (URL . https://api.github.com/users/tdrhq) (HTML--URL . https://github.com/tdrhq) (FOLLOWERS--URL . https://api.github.com/users/tdrhq/followers) (FOLLOWING--URL . https://api.github.com/users/tdrhq/following{/other_user}) (GISTS--URL . https://api.github.com/users/tdrhq/gists{/gist_id}) (STARRED--URL . https://api.github.com/users/tdrhq/starred{/owner}{/repo}) (SUBSCRIPTIONS--URL . https://api.github.com/users/tdrhq/subscriptions) (ORGANIZATIONS--URL . https://api.github.com/users/tdrhq/orgs) (REPOS--URL . https://api.github.com/users/tdrhq/repos) (EVENTS--URL . https://api.github.com/users/tdrhq/events{/privacy}) (RECEIVED--EVENTS--URL . https://api.github.com/users/tdrhq/received_events) (TYPE . User) (SITE--ADMIN)) (INSTALLATION (ID . 13974089) (NODE--ID . MDIzOkludGVncmF0aW9uSW5zdGFsbGF0aW9uMTM5NzQwODk=)))
 <INFO> [19:23:15] hex better-easy-handler.lisp () - 127.0.0.1 (140.82.115.242)

|#
