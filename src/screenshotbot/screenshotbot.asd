(defsystem :screenshotbot
  :serial t
  :author "Arnold Noronha <arnold@screenshotbot.io>"
  :license "Mozilla Public License, v 2.0"
  :depends-on (:util
               :markup
               :gravatar
               :pkg
               :auth
               :jvm
               #-screenshotbot-oss
               :sentry
               :server
               :java.libs
               :util/form-state
               :util/hash-lock
               :jose
               :trivial-file-size
               :screenshotbot.js-assets
               :oidc
               :screenshotbot.css-assets
               :screenshotbot/secrets
               :util.java
               :util/phabricator
               :hunchensocket
               :dexador
               :opticl
               :anaphora
               :dag
               :quri
               :clavier
               :cl-cron
               :cl-interpol
               :dns-client
               :random-sample
               :pem
               ;;:cljwt-custom ;; custom comes from clath, for rs-256
               :do-urlencode
               :nibble
               :cl-json)
  :components
  ((:file "injector")
   (:file "ignore-and-log-errors")
   (:file "analytics" :depends-on ("ignore-and-log-errors"))
   (:file "plugin")
   (:file "mailer")
   (:file "magick")
   (:file "installation")
   (:file "server" :depends-on ("analytics"))
   (:file "cdn")
   (:file "google-fonts")
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
                 (:file "simple-card-page")
                 (:file "confirmation-page")
                 (:file "all")))
   (:file "artifacts")
   (:file "assets")
   (:file "git-repo")
   (:module "model"
    :serial t
    :components ((:file "core")
                 (:file "company")
                 (:file "user")
                 (:file "invite")
                 (:file "github")
                 (:file "view")
                 (:file "recorder-run")
                 (:file "report" :depends-on ("recorder-run"))
                 (:file "image")
                 (:file "channel")
                 (:file "screenshot")
                 (:file "api-key")
                 (:file "commit-graph")
                 (:file "test-object")
                 (:file "note")
                 (:file "all")))
   (:file "impersonation")
   (:file "diff-report")
   (:module "dashboard"
    :serial t
    :components ((:file "home")
                 (:file "paginated")
                 (:file "numbers")
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
                 (:file "history")
                 (:file "mask-builder")
                 (:file "site-admin")))
   (:file "invite")
   (:module "documentation"
    :components ((:file "doc")))
   (:file "image-comparison")
   (:module "github"
    :serial t
    :components ((:file "plugin")
                 (:file "github-installation")
                 (:file "marketplace")
                 (:file "webhook")
                 (:file "jwt-token")
                 (:file "access-checks")
                 (:file "pr-checks" :depends-on ("access-checks"))
                 (:file "pull-request-promoter")
                 (:file "settings")
                 (:file "task-integration")
                 (:file "all")))
   (:module "phabricator"
    :serial t
    :components ((:file "plugin")
                 (:file "commenting-promoter")
                 (:file "diff-promoter")
                 (:file "settings")
                 (:file "all")))
   (:module "gitlab"
    :serial t
    :components ((:file "repo")
                 (:file "plugin")
                 (:file "merge-request-promoter")
                 (:file "all")))
   (:module "api"
    :serial t
    :components ((:file "core")
                 (:file "image")
                 (:file "promote")
                 (:file "recorder-run" :depends-on ("promote"))
                 (:file "commit-graph")))
   (:module "login"
    :serial t
    :components ((:file "common")
                 (:file "oidc")
                 (:file "github-oauth")
                 (:file "github-oauth-ui")
                 (:file "google-oauth")
                 (:file "login")
                 (:file "populate")
                 (:file "signup")
                 (:file "forgot-password")))
   (:module "company"
    :serial t
    :components ((:file "new")
                 (:file "members")))
   #+ (or ccl lispworks)
   (:module "slack"
    :serial t
    :components ((:file "plugin")
                 (:file "core")
                 (:file "task-integration")
                 (:file "settings")
                 (:file "all")))
   (:module "settings"
    :serial t
    :components ((:file "settings-template")
                 (:file "general")
                 (:file "security")))
   (:module "admin"
    :serial t
    :components ((:file "core")
                 (:file "index")))
   (:module "tasks"
    :serial t
    :components ((:file "common")))
   (:file "config")
   (:file "package")))


(defsystem :screenshotbot/tests
  :serial t
  :depends-on (:fiveam
               :util
               :util/fiveam
               :fiveam-matchers
               :screenshotbot/utils
               :screenshotbot/replay
               :tmpdir
               :screenshotbot)
  :components ((:file "testing")
               (:file "factory")
               (:file "test-diff-report")
               (:file "test-mailer")
               (:file "test-installation")
               (:file "test-assets")
               (:file "test-template")
               (:file "test-taskie")
               (:module "dashboard"
                :components ((:file "test-recent-runs")
                             (:file "test-api-keys")
                             (:file "test-image")
                             (:file "test-channels")
                             (:file "test-history")))
               (:module "login"
                :components ((:file "test-github-oauth")
                             (:file "test-signup")))
               (:module "model"
                :components ((:file "test-screenshot")
                             (:file "test-user")
                             (:file "test-channel")
                             (:file "test-company")
                             (:file "test-image")
                             (:file "test-commit-graph")
                             (:file "test-acceptable")))
               (:module "github"
                :components ((:file "test-jwt-token")
                             (:file "test-plugin")
                             (:file "test-access-checks")
                             (:file "test-pull-request-promoter")
                             (:file "test-webhook")))
               (:module "replay"
                :components ((:file "test-core")))
               #+ (or ccl lispworks)
               (:module "slack"
                :components ((:file "test-settings")))
               (:module "api"
                :components ((:file "test-image")
                             (:file "test-promote")
                             (:file "test-send-tasks")
                             (:file "test-recorder-runs")))))

(defsystem :screenshotbot/secrets
  :serial t
  :depends-on (:alexandria
               :pkg)
  :components ((:file "secret")
               (:file "artifacts-secrets")))


(defsystem :screenshotbot/store-tests
    :serial t
    :depends-on (:screenshotbot
                 :fiveam)
    :components ((:file "test-store")))


(defsystem :screenshotbot/utils
  :serial t
  :depends-on (:drakma
               :flexi-streams
               :ironclad
               :screenshotbot/secrets
               :md5
               :log4cl
               :cl-fad
               :alexandria)
  :components ((:file "utils")))

(asdf:defsystem :screenshotbot/replay
  :serial t
  :depends-on (:plump
               :lquery
               :uuid
               :cl-store
               :auto-restart
               :dexador
               :json-mop
               :alexandria)
  :components ((:module "replay"
                :serial t
                :components ((:file "core")
                             (:file "browser-config")))))
