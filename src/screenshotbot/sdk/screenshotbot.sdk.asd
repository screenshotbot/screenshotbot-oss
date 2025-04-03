;; Copyright 2018-Present Modern Interpreters Inc.
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.


(defsystem :screenshotbot.sdk/library
  :serial t
  :version "2.12.1"
  :depends-on (:com.google.flag
               :pkg
               :quri
               :ironclad/core
               :core.cli
               :cl-json
               :cxml
               :log4cl
               :util/request
               :easy-macros
               :screenshotbot.sdk/common-flags
               #-screenshotbot-oss
               :sentry
               :sentry-client
               :clingon
               :alexandria
               :auto-restart
               :util/misc
               :util/health-check
               :cl-fad
               :zip
               :trivial-garbage
               :trivial-features
               :util/threading
               :tmpdir
               :imago
               :imago/pngload
               :md5
               :screenshotbot/api-model
               :dag
               :anaphora
               :str)
  :components ((:file "package")
               (:file "flags")
               (:file "api-context")
               (:file "hostname")
               (:file "backoff")
               (:file "version-check")
               (:file "bundle")
               (:file "firebase")
               (:file "android")
               (:file "git")
               (:file "env")
               (:file "run-context")
               (:file "sdk")
               (:file "active-run")
               (:file "health-checks")
               (:file "sentry")
               (:file "install")
               (:file "fetch-run")
               (:file "clingon-api-context")
               (:file "upload-commit-graph")
               (:file "batch")
               (:file "cli-common")
               (:module "dev"
                :components ((:file "record-verify")))
               (:file "failed-run")
               (:file "unchanged-run")
               (:file "finalized-commit")))

(defsystem :screenshotbot.sdk/gradle
  :serial t
  :depends-on (:screenshotbot.sdk/library
               :tar/extract)
  :components ((:file "adb-puller")
               #+lispworks
               (:file "gradle")))


(defsystem :screenshotbot.sdk/static
  :serial t
  :depends-on (:screenshotbot.sdk/library
               :file-lock
               :util/random-port
               :cl-store
               :screenshotbot/replay-core)
  :components ((:file "static")))

(defsystem :screenshotbot.sdk
  :serial t
  :author "Arnold Noronha <arnold@screenshotbot.io>"
  :license "Mozilla Public License, v 2.0"
  :depends-on (:screenshotbot.sdk/library
               :screenshotbot.sdk/static)
  :components ((:file "help")
               (:file "main")))

(defsystem :screenshotbot.sdk/common-flags
  :serial t
  :depends-on (#:com.google.flag)
  :components ((:file "common-flags")))


(defsystem :screenshotbot.sdk/tests
  :serial t
  :depends-on (:screenshotbot.sdk
               :fiveam
               :fiveam-matchers
               :screenshotbot
               :screenshotbot/testing-lib
               :util/hunchentoot-engine
               :util/fake-clingon
               :util/fiveam)
  :components ((:file "integration-fixture")
               (:file "test-bundle")
               (:file "test-backoff")
               (:file "test-installer")
               (:file "test-unchanged-run")
               (:file "test-clingon-api-context")
               (:file "test-version-check")
               (:file "test-main")
               (:file "test-active-run")
               (:file "test-run-context")
               (:file "test-flags")
               (:file "test-git")
               (:file "test-fetch-run")
               (:file "test-env")
               (:module "dev"
                :components ((:file "test-record-verify")))
               (:file "test-sentry")
               (:file "test-cli-common")
               (:file "test-android")
               (:file "test-sdk")
               (:file "test-firebase")
               (:file "test-static")))


(defsystem :screenshotbot.sdk/deliver
  :author "Arnold Noronha <arnold@screenshotbot.io>"
  :license "Mozilla Public License, v 2.0"
  :defsystem-depends-on (#:build-utils/deliver-script)
  :depends-on (:screenshotbot.sdk)
  :components (("build-utils/deliver-script:deliver-script"
                "deliver-sdk")
               #- (or mswindows win32)
               ("build-utils/deliver-script:makeself-component" "installer"
                                   :depends-on ("deliver-sdk")
                                   :type "sh"
                                   :label "screenshotbot-installer"
                                   :archive ("deliver-sdk"
                                             "installer")
                                   :startup-component "installer")))

#+lispworks
(defsystem :screenshotbot.sdk/deliver-java
  :defsystem-depends-on (#:build-utils/deliver-script)
  :depends-on (#:screenshotbot.sdk/gradle)
  :components (("build-utils/deliver-script:deliver-script"
                "deliver-java")))

(defsystem :screenshotbot.sdk/integration-tests
  :depends-on (#:fiveam
               #:screenshotbot.sdk/deliver
               #:util/testing
               #:screenshotbot/testing-lib
               #:screenshotbot
               #:screenshotbot.sdk)
  :components ((:file "integration-tests")))

(defsystem :screenshotbot.sdk/sdk-integration-tests-impl
  :depends-on (#:alexandria
               #:screenshotbot.sdk/deliver
               #:secure-random
               #:tmpdir
               #:cl-fad)
  :components ((:file "sdk-integration-tests-impl")))
