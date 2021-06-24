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
               :jose
               :screenshotbot.js-assets
               :screenshotbot.css-assets
               :screenshotbot.java
               :opticl
               :anaphora
               :dag
               :quri
               :clavier
               :cl-cron
               :cl-interpol
               :random-sample
               :pem
               ;;:cljwt-custom ;; custom comes from clath, for rs-256
               :crash-logs
               :screenshotbot/utils
               :do-urlencode
               :documentation-plugin
               :nibble
               :cl-json)
  :components
  ((:file "ignore-and-log-errors")
   (:file "analytics" :depends-on ("ignore-and-log-errors"))
   (:file "plugin")
   (:file "installation")
   (:file "secret")
   (:file "server" :depends-on ("analytics"))
   (:file "cdn")
   (:file "form-errors")
   (:file "google-fonts")
   (:file "user-api")
   (:file "notice-api")
   (:file "api-key-api")
   (:file "report-api")
   (:file "promote-api")
   (:file "screenshot-api")
   (:file "settings-api")
   (:file "task-integration-api")
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

   ;; single package from here onwards
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
                 (:file "all")))
   (:module "dashboard"
    :serial t
    :components ((:file "home")
                 (:file "numbers")
                 (:file "run-page")
                 (:file "recent-runs")
                 (:file "notices")
                 (:file "api-keys")
                 (:file "channels")
                 (:file "reports")
                 (:file "history")
                 (:file "image")
                 (:file "mask-builder")))
   (:file "invite")
   (:module "documentation"
    :components ((:file "doc")))
   (:file "compare")
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
                 (:file "github-oauth")
                 (:file "github-oauth-ui")
                 (:file "google-oauth")
                 (:file "login")
                 (:file "populate")
                 (:file "signup")
                 (:file "forgot-password")))
   (:module "company"
    :serial t
    :components ((:file "new")))
   #+lispworks
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
   (:module "tasks"
    :serial t
    :components ((:file "common")))
   (:file "package")))


(defsystem :screenshotbot/tests
  :serial t
  :depends-on (:fiveam
               :util
               :tmpdir
               :screenshotbot)
  :components ((:file "testing")
               (:file "factory")
               (:file "test-compare")
               (:file "test-installation")
               (:file "test-assets")
               (:file "test-template")
               (:file "test-taskie")
               (:module "dashboard"
                :components ((:file "test-recent-runs")
                             (:file "test-api-keys")
                             (:file "test-channels")
                             (:file "test-history")))
               #+ (or ccl lispworks)
               (:module "login"
                :components (#+lispworks (:file "test-github-oauth")
                             (:file "test-signup")))
               (:module "model"
                :components ((:file "test-screenshot")
                             (:file "test-channel")
                             (:file "test-image")
                             (:file "test-commit-graph")
                             (:file "test-acceptable")))
               #+lispworks
               (:module "github"
                :components ((:file "test-jwt-token")
                             (:file "test-access-checks")
                             (:file "test-pull-request-promoter")
                             (:file "test-webhook")))
               #+lispworks
               (:module "slack"
                :components ((:file "test-settings")))
               (:module "api"
                :components (#+lispworks (:file "test-image")
                             #+lispworks (:file "test-promote")
                             #+lispworks (:file "test-send-tasks")
                             #+lispworks (:file "test-recorder-runs")))))


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
               :md5
               :log4cl
               :cl-fad
               :alexandria)
  :components ((:file "utils")))
