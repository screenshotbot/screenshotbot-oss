#+nil
(defpackage :screenshotbot
  (:use #:cl
        #:alexandria
        #:documentation-plugin
        #:util/java
        #:screenshotbot/server
        #:screenshotbot/secrets
        #:screenshotbot/cdn
        #:screenshotbot/form-errors
        #:screenshotbot/chatwoot
        #:screenshotbot/template
        #:screenshotbot/google-fonts
        #:screenshotbot/landing-template
        #:screenshotbot/pricing
        #:screenshotbot/user-api
        #:screenshotbot/left-side-bar
        #:screenshotbot/promote-api
        #:screenshotbot/taskie
        #:screenshotbot/dashboard/numbers
        #:screenshotbot/dashboard/recent-runs
        #:screenshotbot/ui
        #:screenshotbot/notice-api
        #:screenshotbot/api-key-api
        #:screenshotbot/report-api
        #:screenshotbot/dashboard/reports
        #:screenshotbot/dashboard/notices
        #:screenshotbot/screenshot-api
        #:screenshotbot/artifacts
        #:screenshotbot/invite
        #:screenshotbot/git-repo
        #:screenshotbot/model
        #:screenshotbot/github
        #:screenshotbot/compare
        #:screenshotbot/api/core
        #:screenshotbot/api/image
        #:screenshotbot/api/promote
        #:screenshotbot/api/recorder-run
        #:screenshotbot/login/common
        #:screenshotbot/login/github-oauth
        #:screenshotbot/login/google-oauth
        #:screenshotbot/login/login
        #:screenshotbot/login/signup
        #:screenshotbot/login/populate
        #:screenshotbot/settings-api
        #:screenshotbot/admin/tracking-emails
        #:screenshotbot/task-integration-api
        #:screenshotbot/asana/core
        #:screenshotbot/trello/core
        #:screenshotbot/trello/settings
        #+lispworks
        #:screenshotbot/jira/core
        #:screenshotbot/dashboard/run-page
        #:screenshotbot/pages/support
        #+lispworks
        #:screenshotbot/slack/core)
  (:nicknames #:sb)
  (:import-from #:markup
                #:deftag)
  (:import-from #:nibble
                #:nibble)
  (:import-from #:anaphora
                #:it
                #:awhen)
  (:import-from #:hunchentoot
                #:*catch-errors-p*)
  (:import-from #:bknr.datastore
                #:store-object
                #:blob-pathname
                #:*store*
                #:persistent-class
                #:store-objects-with-class
                #:snapshot
                #:deftransaction
                #:initialize-transient-instance
                #:hash-index
                #:store-object-id
                #:store-object-with-id
                #:store-objects-with-class
                #:object-destroyed-p
                #:delete-object
                #:unique-index
                #:in-transaction-p
                #:with-transaction
                #:all-store-objects
                #:slot-index)
  (:import-from #:util
                #:make-url
                #:*delivered-image*
                #:object-with-oid
                #:oid
                #:find-by-oid
                #:make-secret-code)
  (:import-from #:mquery
                #:$)
  (:import-from #:screenshotbot/ignore-and-log-errors
                #:ignore-and-log-errors)
  (:import-from #:screenshotbot/analytics
                #:push-analytics-event)
  (:export #:*acceptor*
           #:*thread-pool*
           #:pull-request-promoter
           #:prepare-delivered-asset-map
           #:*delivered-asset-compile-map*
           #:maybe-promote
           #:current-user
           #:current-company
           #:make-stripe-customer
           #:stripe-customer
           #:jira-config
           #:*root*
           #:user-full-name
           #:user-email
           #:user
           #:admin-app-template
           #:maybe-send-tasks
           #:defadminhandler
           #:defhandler
           #:professionalp
           #:staging-p
           #:simple-card-page
           #:app-template
           #:plan-selection
           #:customer-id
           #:pro-plan-signup
           #:recorder-run
           #:github-installation
           #:promoter
           #:all-users
           #:update-pull-request
           #:pull-request-url
           #:phabricator-promoter
           #:make-diff-report
           #:diff-report-title
           #:report-page
           #:production-run-for
           #:diff-report-empty-p
           #:github-update-pull-request
           #:github-repo-full-name
           #:recorder-run-channel
           #:recorder-run-commit
           #:github-installation-with-repo-name
           #:image-public-url
           #:with-login
           #:confirmation-page
           #:timeago
           #:base-acceptable
           #:tracked-url
           #:tracked-url-click
           #:open-image-stream
           #:ignore-and-log-errors
           #:adminp
           #:github-repos-for-user
           #:acceptable-state
           #:*disable-mail*
           #:new-instance
           #:make-gitlab-repo
           #:get-parent-commit
           #:repo-left-ancestor-p
           #:public-repo-p
           #:report-acceptable
           #:screenshot-image
           #:java-list->list
           #:*domain*
           #:with-local-image
           #:gitlab-merge-request-iid
           #:update-reuben-ip
           #:commenting-promoter
           #:*btrue*
           #:*bfalse*
           #:java-syntax))


;; These symbols are for backward compatibility for bknr.datastore. If
;; you're reading this, and don't know what it's used for, it's
;; probably not needed. Essentially if a class has a slot
;; screenshotbot::xyz, and we moved that class to screenshotbot/foo,
;; so the slot becomes screenshotbot/foo::xyz: then in order to read
;; the slot xyz into the new package, the symbol must exist in the
;; main package. As long as the symbol exists, everything will be
;; automated.
#+nil
(loop for x in `(#:admins #:channels #:demo-filled-p
                          #:images #:invites
                          #:owner
                          #:reports #:runs
                          #:gh-user-id
                          #:expires-in
                          #:refresh-token
                          #:refresh-token-expires-in
                          #:branch-hash
                          #:build-url
                          #:commit-hash
                          #:merge-base-hash
                          #:height
                          #:left
                          #:top
                          #:width
                          #:num-changes
                          #:active-run
                          #:expired-p
                          #:payload
                          #:secret
                          #:signature
                          #:verified-p
                          #:base
                          #:head
                          #:pull-id
                          #:repo-name
                          #:user-id
                          #:created-at
                          #:clicks
                          #:ts
                          #:trello-token
                          #:token
                          #:message
                          #:name
                          #:subject)
      do
      (intern (string x) "SCREENSHOTBOT"))
