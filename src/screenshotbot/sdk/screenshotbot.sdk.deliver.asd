;; Copyright 2018-Present Modern Interpreters Inc.
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot.sdk.deliver
  (:use :cl :asdf))
(in-package :screenshotbot.sdk.deliver)


(defsystem :screenshotbot.sdk.deliver
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
(defsystem :screenshotbot.sdk.deliver/java-so
  :components (("SCREENSHOTBOT/PLATFORM-ASSET:DELIVER-SO-SCRIPT"
                "deliver-java-so")))
