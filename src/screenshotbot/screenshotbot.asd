;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot-system
  (:use :cl
   :asdf))
(in-package :screenshotbot-system)


(defsystem :screenshotbot/events
  :serial t
  :defsystem-depends-on (:trivial-features)
  :depends-on (:clsql
               :util/clsql
               :core.installation
               :util/misc
               :util/atomics
               :atomics)
  :components ((:file "events")))

(defsystem :screenshotbot/mask-rect-api
  :serial t
  :depends-on ()
  :components ((:file "mask-rect-api")))

(defsystem :screenshotbot
  :serial t
  :author "Arnold Noronha <arnold@screenshotbot.io>"
  :license "Mozilla Public License, v 2.0"
  :defsystem-depends-on (:trivial-features)
  :depends-on (:util
               :markup
               :gravatar
               :util/posix
               :bknr.impex
               :util/logger
               :lparallel
               :util/lparallel
               (:feature (:not :screenshotbot-oss) :documentation-plugin)
               :cl-store
               :core.installation
               :sentry-client
               :fset
               :trivia
               :json-mop
               :encrypt
               :util/timeago
               :iterate
               :pkg
               #-lispworks
               :util/fake-fli
               :auth
               #+(or ccl lispworks)
               :jvm
               #-screenshotbot-oss
               :sentry
               :server
               :core.ui
               :auto-restart
               :scheduled-jobs
               :serapeum
               #+(or ccl lispworks)
               :java.libs
               :screenshotbot/events
               :util/form-state
               :util/hash-lock
               :util/request
               :jose
               :util/digests
               :trivial-file-size
               :screenshotbot.js-assets
               :oidc
               :cl-isaac
               :screenshotbot.css-assets
               :screenshotbot/secrets
               :util.java
               :util/phabricator
               :util/copy-file
               :hunchensocket
               :drakma
               :anaphora
               :dag
               #-screenshotbot-oss
               :sentry-client
               :quri
               :clavier
               :easy-macros
               :cl-cron
               :cl-interpol
               :dns-client
               :gzip-stream
               :zs3
               :random-sample
               :gatekeeper
               :pem
               ;;:cljwt-custom ;; custom comes from clath, for rs-256
               :do-urlencode
               :screenshotbot.magick
               :screenshotbot/api-model
               :nibble
               :cl-json)
  :components
  ((:static-file "dtd/api" :type "dtd")
   (:file "injector")
   (:file "ignore-and-log-errors")
   (:file "analytics" :depends-on ("ignore-and-log-errors"))
   (:file "plugin")
   (:file "async")
   (:file "mailer")
   (:file "installation")
   (:file "server" :depends-on ("analytics"))
   (:module "s3"
    :serial t
    :components ((:file "core")
                 (:file "backup")))
   (:file "cdn")
   (:file "user-api")
   (:file "notice-api")
   (:file "api-key-api")
   (:file "report-api")
   (:file "promote-api")
   (:file "screenshot-api")
   (:file "settings-api")
   (:file "task-integration-api")
   (:file "plan")
   (:file "template")
   (:file "left-side-bar")
   (:file "taskie")
   (:module "ui"
    :components ((:file "core")
                 (:file "confirmation-page")
                 (:file "all")))
   (:file "artifacts")
   (:file "assets")
   (:file "git-repo")
   (:module "model"
    :serial t
    :components ((:file "core")
                 (:file "transient-object")
                 (:file "auto-cleanup")
                 (:file "company")
                 (:file "sharing")
                 (:file "user")
                 (:file "invite")
                 (:file "github")
                 (:file "view")
                 (:file "image")
                 (:file "screenshot-key")
                 (:file "screenshot-map")
                 (:file "recorder-run")
                 (:file "batch")
                 (:file "report" :depends-on ("recorder-run"))
                 (:file "image-comparison")
                 (:file "channel")
                 (:file "failed-run")
                 (:file "image-comparer")
                 (:file "finalized-commit")
                 (:file "screenshot")
                 (:file "api-key")
                 (:file "commit-graph")
                 (:file "test-object")
                 (:file "note")
                 (:file "all")))
   (:file "audit-log")
   (:file "impersonation")
   (:file "diff-report")
   (:file "invite")
   (:module "dashboard"
    :serial t
    :components ((:file "explain")
                 (:file "home")
                 (:file "ensure-company")
                 (:file "review-link")
                 (:file "run-page")
                 (:file "image")
                 (:file "compare")
                 (:file "notes")
                 (:file "recent-runs")
                 (:file "notices")
                 (:file "new-compare")
                 (:file "api-keys")
                 (:file "channels")
                 (:file "reports")
                 (:file "bisect")
                 (:file "history")
                 (:file "mask-builder")
                 (:file "site-admin")
                 (:file "audit-log")))
   (:file "image-comparison")
   (:module "webhook"
    :serial t
    :components ((:file "model")
                 (:file "webhook")
                 (:file "settings")))
   (:file "abstract-pr-promoter")
   (:module "github"
    :serial t
    :components ((:file "plugin")
                 (:file "github-installation")
                 (:file "audit-log")
                 (:file "marketplace")
                 (:file "webhook")
                 (:file "jwt-token")
                 (:file "access-checks")
                 (:file "review-link")
                 (:file "app-installation")
                 (:file "pr-checks" :depends-on ("access-checks"))
                 (:file "read-repos")
                 (:file "settings")
                 (:file "pull-request-promoter")
                 (:file "task-integration")
                 (:file "all")))
   (:module "azure"
    :serial t
    :components ((:file "request")
                 (:file "plugin")
                 (:file "settings")
                 (:file "promoter")))
   (:module "bitbucket"
    :components ((:file "core")
                 (:file "plugin")
                 (:file "review-link")
                 (:file "audit-log")
                 (:file "settings")
                 (:file "promoter")))
   (:module "gitlab"
    :serial t
    :components ((:file "repo")
                 (:file "audit-logs")
                 (:file "plugin")
                 (:file "review-link")
                 (:file "settings")
                 (:file "merge-request-promoter")
                 (:file "all")))
   (:module "api"
    :serial t
    :components ((:file "core")
                 (:file "version")
                 (:file "failed-run")
                 (:file "unchanged-run")
                 (:file "finalized-commit")
                 (:file "image")
                 (:file "promote")
                 (:file "recorder-run" :depends-on ("promote"))
                 (:file "commit-graph")))
   (:module "phabricator"
    :serial t
    :components ((:file "plugin")
                 (:file "builds")
                 (:file "diff-promoter")
                 (:file "settings")
                 (:file "all")))
   (:module "sso"
    :serial t
    :components ((:file "model")
                 (:file "fake")))
   (:module "login"
    :serial t
    :components ((:file "common")
                 (:file "oidc")
                 (:file "github-oauth")
                 (:file "github-oauth-ui")
                 (:file "google-oauth")
                 (:file "aws-cognito")
                 (:file "login")
                 (:file "populate")
                 (:file "signup")
                 (:file "forgot-password")))
   (:module "company"
    :serial t
    :components ((:file "new")
                 (:file "members")
                 (:file "rename")))
   (:module "slack"
    :serial t
    :components ((:file "plugin")
                 (:file "core")
                 (:file "task-integration")
                 (:file "settings")
                 (:file "all")))
   (:module "email-tasks"
    :components ((:file "settings")
                 (:file "task-integration")))
   (:module "settings"
    :serial t
    :components ((:file "settings-template")
                 (:file "general")
                 (:file "security")
                 (:file "shares")))
   (:module "admin"
    :serial t
    :components ((:file "core")
                 (:file "index")
                 (:file "site-info")))
   (:module "tasks"
    :serial t
    :components ((:file "common")))
   (:file "config")
   (:file "cleanup")))

(defsystem :screenshotbot/testing-lib
  :serial t
  :depends-on (:screenshotbot
               :util/fiveam
               :util/testing
               :alexandria
               :fiveam-matchers)
  :components ((:file "testing")))

(defsystem :screenshotbot/tests
  :serial t
  :depends-on (:fiveam
               :util
               :util/fiveam
               :fiveam-matchers
               :util/random-port
               :trivia
               :screenshotbot/utils
               :screenshotbot/replay-core
               :screenshotbot/webdriver
               :screenshotbot/replay
               :screenshotbot/testing-lib
               :screenshotbot/events
               :tmpdir
               :cl-mock
               :screenshotbot)
  :components ((:file "factory")
               (:file "test-server")
               (:file "test-artifacts")
               (:file "test-promote-api")
               (:file "test-diff-report")
               (:file "test-events" :if-feature (:not :windows))
               (:file "test-mailer")
               (:file "test-secret")
               (:file "test-async")
               (:file "test-settings-api")
               (:file "test-config")
               (:file "test-installation")
               (:file "test-assets")
               (:file "test-template")
               (:file "test-taskie")
               (:file "test-audit-log")
               (:file "test-abstract-pr-promoter")
               (:module "webhook"
                :components ((:file "test-model")
                             (:file "test-webhook")
                             (:file "test-settings")))
               (:module "dashboard"
                :components ((:file "test-recent-runs")
                             (:file "test-ensure-company")
                             (:file "test-review-link")
                             (:file "test-run-page")
                             (:file "test-compare")
                             (:file "test-reports")
                             (:file "test-api-keys")
                             (:file "test-image")
                             (:file "test-channels")
                             (:file "test-history")
                             (:file "test-bisect")
                             (:file "test-notices")))
               (:module "bitbucket"
                :components ((:static-file "error-response-1" :type "json")
                             (:file "test-plugin")
                             (:file "test-audit-log")
                             (:file "test-review-link")
                             (:file "test-settings")
                             (:file "test-promoter")))
               (:module "login"
                :components ((:file "test-github-oauth")
                             (:file "test-google-oauth")
                             (:file "test-populate")
                             (:file "test-oidc")
                             (:file "test-login")
                             (:file "test-signup")
                             (:file "test-common")))
               (:module "model"
                :components ((:file "test-core")
                             (:file "test-auto-cleanup")
                             (:file "test-transient-object")
                             (:file "test-recorder-run")
                             (:file "test-batch")
                             (:file "test-finalized-commit")
                             (:file "test-screenshot")
                             (:file "test-screenshot-key")
                             (:file "test-screenshot-map")
                             (:file "test-image-comparer")
                             (:file "test-report")
                             (:file "test-user")
                             (:file "test-channel")
                             (:file "test-api-key")
                             (:file "test-company")
                             (:file "test-image")
                             (:file "test-image-comparison")
                             (:file "test-commit-graph")
                             (:file "test-acceptable")))
               (:module "webdriver"
                :components ((:file "test-screenshot")))
               (:module "github"
                :components ((:file "test-jwt-token")
                             (:file "test-app-installation")
                             (:file "test-read-repos")
                             (:file "test-plugin")
                             (:file "test-access-checks")
                             (:file "test-pull-request-promoter")
                             (:file "test-review-link")
                             (:file "test-webhook")))
               (:module "gitlab"
                :components ((:file "test-merge-request-promoter")))
               (:module "azure"
                :components ((:file "test-plugin")
                             (:file "test-settings")
                             (:file "test-promoter")))
               (:module "phabricator"
                :components ((:file "test-builds")
                             (:file "test-diff-promoter")))
               (:module "replay"
                :components ((:file "test-core")
                             (:file "test-sitemap")
                             (:file "test-remote")
                             (:file "test-run-builder")
                             (:file "test-integration")
                             (:file "test-replay-acceptor")))
               (:module "slack"
                :components ((:file "test-settings")
                             (:file "test-task-integration")))
               (:file "test-analytics" :if-feature (:not :windows))
               (:module "email-tasks"
                :components ((:file "test-task-integration")))
               (:module "settings"
                :components ((:file "test-security")))
               (:module "api"
                :components ((:file "test-core")
                             (:file "test-model")
                             (:file "test-failed-run")
                             (:file "test-unchanged-run")
                             (:file "test-finalized-commit")
                             (:file "test-commit-graph")
                             (:file "test-image")
                             (:file "test-promote")
                             (:file "test-send-tasks")
                             (:file "test-recorder-runs")))
               (:module "web-build"
                :components ((:file "test-scheduler")
                             (:file "test-project")))))

(defsystem :screenshotbot/secrets
  :serial t
  :depends-on (:alexandria
               :pkg
               :util/digests
               :bknr.impex)
  :components ((:static-file "dtd/secret" :type "dtd")
               (:file "secret")
               (:file "artifacts-secrets")))


(defsystem :screenshotbot/store-tests
  :serial t
  :depends-on (:screenshotbot
               :util/testing
               :fiveam)
  :components ((:file "test-store")))


(defsystem :screenshotbot/utils
  :serial t
  :depends-on (:drakma
               :flexi-streams
               :ironclad
               :screenshotbot/secrets
               :easy-macros
               :gzip-stream
               :md5
               :log4cl
               :cl-fad
               :alexandria)
  :components ((:file "utils")))

(asdf:defsystem :screenshotbot/replay-core
  :serial t
  :depends-on (:plump
               :lquery
               :fset
               :uuid
               :easy-macros
               :cl-store
               :util/threading
               :util/misc
               :util/request
               :util/lru-cache
               :util/json-mop
               :hunchentoot-extensions
               :util/digests
               :util/cron
               :screenshotbot/hub
               :http-proxy
               :auto-restart
               :drakma
               :json-mop
               :alexandria)
  :components ((:module "replay"
                :serial t
                :components ((static-file "replay-regex" :type "txt")
                             (:file "browser-config")
                             (:file "core")
                             (:file "squid")))))



(asdf:defsystem :screenshotbot/replay
  :serial t
  :depends-on (:dexador
               :cl+ssl
               :cl-mongo-id
               :cl-webdriver-client
               :hunchentoot
               :hunchensocket
               :screenshotbot
               :tmpdir
               :quri
               :screenshotbot/replay-core
               :ironclad/core
               :cl-json
               :scale
               :ironclad/digests
               :screenshotbot.sdk
               :str
               :flexi-streams
               :scheduled-jobs
               :xmls
               :lquery
               :drakma
               :markup
               :screenshotbot/webdriver
               :screenshotbot/hub
               :closer-mop
               :plump)
  :components ((:module "replay"
                :serial t
                :components ((:file "proxy")
                             (:file "replay-acceptor")
                             (:file "services")
                             (:file "frontend")
                             (:file "sitemap")
                             (:file "run-builder")
                             (:file "integration")
                             (:file "remote")))
               (:module "web-build"
                :serial t
                :components ((:file "project")
                             (:file "scheduler")
                             (:file "device-list")
                             (:file "browsers")))))

(defsystem :screenshotbot/hub
  :serial t
  :depends-on (:util/request
               :util/misc
               :hunchentoot
               :auto-restart
               :cl-json)
  :components ((:module "hub"
                :components ((:file "server")
                             (:file "container")))))

(defsystem :screenshotbot/api-model
  :serial t
  :depends-on (:json-mop
               :util/json-mop)
  :components ((:module "api"
                :components ((:file "model")))))

(defsystem :screenshotbot/webdriver
  :serial t
  :depends-on (:flexi-streams
               :trivial-file-size
               :auto-restart
               #-lispworks
               :util/fake-fli
               :cl-webdriver-client)
  :components ((:module "webdriver"
                :components ((:file "impl")
                             (:file "screenshot")
                             (:file "all")))))

(defsystem :screenshotbot/all
  :serial t
  :depends-on (:screenshotbot
               :screenshotbot/replay
               :screenshotbot.migrations))
