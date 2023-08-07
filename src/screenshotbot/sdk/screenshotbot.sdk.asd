;; Copyright 2018-Present Modern Interpreters Inc.
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defsystem :screenshotbot.sdk/library
  :serial t
  :version "2.5.28"
  :depends-on (:com.google.flag
               :pkg
               :quri
               :ironclad/core
               :hunchentoot
               :cl-json
               :cxml
               :log4cl
               :util/request
               :easy-macros
               :screenshotbot.sdk/common-flags
               #-screenshotbot-oss
               :sentry
               :sentry-client
               :alexandria
               :auto-restart
               :util/misc
               :util/health-check
               :cl-fad
               :zip
               :trivial-garbage
               :trivial-features
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
               (:file "hostname")
               (:file "backoff")
               (:file "version-check")
               (:file "bundle")
               (:file "firebase")
               (:file "android")
               (:file "env")
               (:file "git")
               (:file "sdk")
               (:file "failed-run")
               (:file "unchanged-run")))

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
               :util/fiveam)
  :components ((:file "test-bundle")
               (:file "test-version-check")
               (:file "test-flags")
               (:file "test-git")
               (:file "test-env")
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
