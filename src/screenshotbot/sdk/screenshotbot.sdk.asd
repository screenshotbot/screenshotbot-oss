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
               :cl-store
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
               :server/interrupts
               :screenshotbot/hub
               :dag
               :anaphora
               :str)
  :components ((:file "package")
               (:file "flags")
               (:file "bundle")
               (:file "android")
               (:file "git")
               (:file "help")
               (:file "sdk")
               (:file "static")
               (:file "selenium")
               (:file "main")))


(defsystem :screenshotbot.sdk/tests
  :serial t
  :depends-on (:screenshotbot.sdk
               :fiveam
               :fiveam-matchers
               :screenshotbot
               :util/fiveam)
  :components ((:file "test-bundle")
               (:file "test-sdk")
               (:file "test-static")))


(defsystem :screenshotbot.sdk/deliver
  :author "Arnold Noronha <arnold@screenshotbot.io>"
  :license "Mozilla Public License, v 2.0"
  :defsystem-depends-on (#:screenshotbot/build-utils)
  :depends-on (:screenshotbot.sdk)
  :components (("SCREENSHOTBOT/PLATFORM-ASSET:DELIVER-SCRIPT"
                "deliver-sdk")
               #- (or mswindows win32)
               ("SCREENSHOTBOT/PLATFORM-ASSET:MAKESELF-COMPONENT" "installer"
                                   :depends-on ("deliver-sdk")
                                   :type "sh"
                                   :label "screenshotbot-installer"
                                   :archive ("deliver-sdk"
                                             "installer")
                                   :startup-component "installer")))

#+lispworks
(defsystem :screenshotbot.sdk/deliver-java-so
  :components (("SCREENSHOTBOT/PLATFORM-ASSET:DELIVER-SO-SCRIPT"
                "deliver-java-so")))
