;; Copyright 2018-Present Modern Interpreters Inc.
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defsystem :screenshotbot.sdk
  :author "Arnold Noronha <arnold@screenshotbot.io>"
  :license "Mozilla Public License, v 2.0"
  :serial t
  :depends-on (:dexador
               :com.google.flag
               :pkg
               :ironclad/core
               :hunchentoot
               :cl-json
               :log4cl
               :util/random-port
               :util/request
               :easy-macros
               :screenshotbot.sdk/common-flags
               #-screenshotbot-oss
               :sentry
               :cl-store
               :util/health-check
               :nyaml
               :cl-fad
               :cxml
               :zip
               :trivial-garbage
               :tmpdir
               :imago
               :imago/pngload
               :md5
               :screenshotbot/replay-core
               :screenshotbot/api-model
               :dag
               :anaphora
               :str)
  :components ((:file "package")
               (:file "flags")
               (:file "version-check")
               (:file "bundle")
               (:file "firebase")
               (:file "android")
               (:file "git")
               (:file "help")
               (:file "sdk")
               (:file "static")
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
(defsystem :screenshotbot.sdk/deliver-java-so
  :defsystem-depends-on (#:build-utils/deliver-script)
  :components (("build-utils/deliver-script:deliver-so-script"
                "deliver-java-so")))
