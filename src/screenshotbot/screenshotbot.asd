(defpackage :screenshotbot-system
  (:use :cl
   :asdf))

(defclass lib-source-file (c-source-file)
  ())

(defparameter *library-file-dir*
  (make-pathname :name nil :type nil
                 :defaults *load-truename*))

(defun default-foreign-library-type ()
  "Returns string naming default library type for platform"
  #+(or win32 win64 cygwin mswindows windows) "dll"
  #+(or macosx darwin ccl-5.0) "dylib"
  #-(or win32 win64 cygwin mswindows windows macosx darwin ccl-5.0) "so"
)

(defmethod output-files ((o compile-op) (c lib-source-file))
  (let ((library-file-type
          (default-foreign-library-type)))
    (list (make-pathname :type library-file-type
                         :defaults (asdf:component-pathname c)))))

(defmethod perform ((o load-op) (c lib-source-file))
  t)

(defun guess-mac-magick-location ()
  (let ((dir "/opt/homebrew/Cellar/imagemagick/"))
    (loop for child in (uiop:subdirectories dir)
          for name = (car (last (pathname-directory child)))
          if (eql #\7 (elt name 0))
            return child)))

(defmethod perform ((o compile-op) (c lib-source-file))
  (uiop:run-program `("gcc"
                      "-shared"
                      ,(namestring
                        (component-pathname c))
                      "-I"
                      ,(cond
                        ((uiop:os-windows-p)
                         (namestring #P"C:/Program Files/ImageMagick-7.1.0-Q8/include/"))
			            ((uiop:os-macosx-p)
		                 (namestring #P"/opt/homebrew/include/ImageMagick-7/"))
                        (t "/usr/local/include/ImageMagick-7/"))
			          ;; It's super difficult to install the D8 version of imagemagick on Mac... maybe in the future
                      "-D" ,(format nil "MAGICKCORE_QUANTUM_DEPTH=~a" (if (uiop:os-macosx-p) 16 8))
                      "-D" ,(format nil "MAGICKCORE_HDRI_ENABLE=~a" (if (uiop:os-macosx-p) 1 0))
                      "-Werror"
                      "-Wall"
                      ,@(cond
                         ((uiop:os-windows-p)
                          (list "-L" (namestring #P"C:/Program Files/ImageMagick-7.1.0-Q8/lib/") "-lCORE_RL_MagickWand_"))
                         ((uiop:os-macosx-p)
                          (list (format nil "-L~alib" (namestring (guess-mac-magick-location)))
                                "-lMagickWand-7.Q16HDRI"))
                         (t
                          (list "-lMagickWand-7.Q8")))
                      "-o" ,(namestring (car (output-files o c))))
                      :output *standard-output*
                      :error-output *error-output*))

(defsystem :screenshotbot
  :serial t
  :author "Arnold Noronha <arnold@screenshotbot.io>"
  :license "Mozilla Public License, v 2.0"
  :depends-on (:util
               :markup
               :gravatar
               (:feature (:not :screenshotbot-oss) :documentation-plugin)
               :cl-store
               :encrypt
               :iterate
               :pkg
               #-lispworks
               :util/fake-fli
               :auth
               :jvm
               #-screenshotbot-oss
               :sentry
               :server
               :auto-restart
               :serapeum
               :java.libs
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
               :osicat
               :util/phabricator
               :hunchensocket
               :drakma
               :anaphora
               :dag
               #-screenshotbot-oss
               :sentry-client
               :quri
               :clavier
               :util/macros
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
   (:module "magick"
    :components ((:file "magick")
                 (lib-source-file "magick-native")
                 (:file "memory" :if-feature :lispworks)
                 (:file "magick-lw")))
   (:file "installation")
   (:file "server" :depends-on ("analytics"))
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
                 (:file "simple-card-page")
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
                 (:file "recorder-run")
                 (:file "report" :depends-on ("recorder-run"))
                 (:file "image")
                 (:file "image-comparison")
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
    :components ((:file "explain")
                 (:file "home")
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
   (:file "image-comparison")
   (:module "github"
    :serial t
    :components ((:file "plugin")
                 (:file "github-installation")
                 (:file "audit-log")
                 (:file "marketplace")
                 (:file "webhook")
                 (:file "jwt-token")
                 (:file "access-checks")
                 (:file "app-installation")
                 (:file "pr-checks" :depends-on ("access-checks"))
                 (:file "read-repos")
                 (:file "settings")
                 (:file "pull-request-promoter")
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
                 (:file "settings")
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
   (:module "email-tasks"
    :components ((:file "settings")
                 (:file "task-integration")))
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
   (:file "package")
   (:file "cleanup")))


(defsystem :screenshotbot/tests
  :serial t
  :depends-on (:fiveam
               :util
               :util/fiveam
               :fiveam-matchers
               :screenshotbot/utils
               :screenshotbot/replay-core
               :screenshotbot/webdriver
               :screenshotbot/replay
               :tmpdir
               :screenshotbot)
  :components ((:file "testing")
               (:file "factory")
               (:file "test-server")
               (:file "test-artifacts")
               (:file "test-diff-report")
               (:file "test-mailer")
               (:module "magick"
                :components ((:file "test-magick-lw")
                             (:file "test-memory" :if-feature :lispworks)))
               (:file "test-installation")
               (:file "test-assets")
               (:file "test-template")
               (:file "test-taskie")
               (:module "dashboard"
                :components ((:file "test-recent-runs")
                             (:file "test-compare")
                             (:file "test-api-keys")
                             (:file "test-image")
                             (:file "test-channels")
                             (:file "test-history")))
               (:module "login"
                :components ((:file "test-github-oauth")
                             (:file "test-signup")
                             (:file "test-common")))
               (:module "model"
                :components ((:file "test-core")
                             (:file "test-auto-cleanup")
                             (:file "test-transient-object")
                             (:file "test-recorder-run")
                             (:file "test-screenshot")
                             (:file "test-user")
                             (:file "test-channel")
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
                             (:file "test-plugin")
                             (:file "test-access-checks")
                             (:file "test-pull-request-promoter")
                             (:file "test-webhook")))
               (:module "replay"
                :components ((:file "test-core")
                             (:file "test-sitemap")
                             (:file "test-integration")
                             (:file "test-replay-acceptor")))
               #+ (or ccl lispworks)
               (:module "slack"
                :components ((:file "test-settings")
                             (:file "test-task-integration")))
               (:file "test-analytics")
               (:module "email-tasks"
                :components ((:file "test-task-integration")))
               (:module "api"
                :components ((:file "test-image")
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
               :util/digests)
  :components ((:file "secret")
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
               :md5
               :log4cl
               :cl-fad
               :alexandria)
  :components ((:file "utils")))

(asdf:defsystem :screenshotbot/replay-core
  :serial t
  :depends-on (:plump
               :lquery
               :uuid
               :util/macros
               :cl-store
               :util/threading
               :util/misc
               :util/request
               :hunchentoot-extensions
               :cl-mongo-id
               :cl-webdriver-client
               :screenshotbot/webdriver
               :cl+ssl
               :util/digests
               :util/cron
               :screenshotbot/hub
               :auto-restart
               :dexador
               :drakma
               :json-mop
               :alexandria)
  :components ((:module "replay"
                :serial t
                :components ((static-file "replay-regex" :type "txt")
                             (:file "core")
                             (:file "browser-config")
                             (:file "squid")
                             (:file "proxy")))))



(asdf:defsystem :screenshotbot/replay
  :serial t
  :depends-on (:dexador
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
                :components ((:file "replay-acceptor")
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
               :screenshotbot/replay))

(defsystem :screenshotbot/build-utils
  :serial t
  :depends-on (:cl-ppcre)
  :components ((:file "platform-asset")))
