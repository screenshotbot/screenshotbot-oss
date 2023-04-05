;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defsystem :screenshotbot.magick
  :serial t
  :defsystem-depends-on (:screenshotbot.magick.build)
  :depends-on (#-lispworks #:util/fake-fli
               #:easy-macros
               #:screenshotbot/events
               #:trivial-features
               #:screenshotbot/mask-rect-api
               #:util/native-module
               #:util/health-check
               #:screenshotbot.magick.build
               #:serapeum
               #:util/copy-file
               #:alexandria)
  :components ((:file "magick")
               ("screenshotbot/magick/build:lib-source-file"
                "magick-native")
               ("screenshotbot/magick/build:magick-cl-source-file" "ffi-7")
               ("screenshotbot/magick/build:magick-cl-source-file" "ffi-6")
               ("screenshotbot/magick/build:magick-cl-source-file"
                "magick-lw")
               (:file "health-checks")))

(defsystem :screenshotbot.magick/tests
  :serial t
  :defsystem-depends-on (:screenshotbot.magick.build)
  :depends-on (:util/fiveam
               :fiveam-matchers
               :util/digests
               :screenshotbot.magick)
  :components (("screenshotbot/magick/build:magick-cl-source-file" "test-magick-lw")))
