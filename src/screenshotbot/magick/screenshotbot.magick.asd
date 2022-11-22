;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defsystem :screenshotbot.magick
  :serial t
  :defsystem-depends-on (:screenshotbot.magick.build)
  :depends-on ((:feature (:not :lispworks) :util/fake-fli)
               :easy-macros
               :screenshotbot/events
               :screenshotbot.magick.build
               :serapeum
               :alexandria)
  :components ((:file "magick")
               ("screenshotbot/magick/build:lib-source-file"
                "magick-native")
               (:file "memory" :if-feature :lispworks)
               (:file "ffi-7" :if-feature :magick-7)
               (:file "ffi-6" :if-feature :magick-6)
               ("screenshotbot/magick/build:magick-cl-source-file" "magick-lw")))

(defsystem :screenshotbot.magick/tests
  :serial t
  :depends-on (:util/fiveam
               :fiveam-matchers
               :util/digests
               :screenshotbot.magick)
  :components ((:file "test-magick-lw")
               (:file "test-memory" :if-feature :lispworks)))
