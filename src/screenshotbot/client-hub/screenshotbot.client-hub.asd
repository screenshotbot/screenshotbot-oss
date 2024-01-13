;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defsystem :screenshotbot.client-hub
  :depends-on (#:screenshotbot.sdk/common-flags
               #:screenshotbot/hub
               #:server/interrupts
               #:screenshotbot/replay)
  :components ((:file "flags")
               (:file "selenium")
               (:file "main")))

(defsystem :screenshotbot.client-hub/deliver
  :defsystem-depends-on (:build-utils/deliver-script)
  :depends-on ()
  :components (("build-utils/deliver-script:deliver-script"
                "deliver-client-hub")))
