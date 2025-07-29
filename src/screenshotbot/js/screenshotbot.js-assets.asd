(defpackage :screenshotbot-system.js-assets
  (:use :cl :asdf))
(in-package :screenshotbot-system.js-assets)

(defsystem :screenshotbot.js-assets/split
  :class "build-utils:js-library"
  :defsystem-depends-on (:build-utils)
  :components (("build-utils:js-file" "split")))

(defsystem screenshotbot.js-assets/headroom
  :class "build-utils:js-library"
  :defsystem-depends-on (:build-utils)
  :depends-on ()
  :components ((:module "vendor"
                :components (("build-utils:js-file" "headroom")))))

(defsystem screenshotbot.js-assets/common
  :class "build-utils:js-library"
  :defsystem-depends-on (:build-utils)
  :depends-on ()
  :components (("build-utils:js-file" "common")))

(defsystem screenshotbot.js-assets/package
  :depends-on (:parenscript
               :3d-matrices)
  :components ((:file "package")))

(defsystem screenshotbot.js-assets
  :class "build-utils:js-system"
  :serial t
  :defsystem-depends-on (:build-utils)
  :depends-on (:screenshotbot.js-assets/package
               #-screenshotbot-oss
               :sentry-js
               :bootstrap5-js
               :screenshotbot.js-assets/headroom
               :screenshotbot.js-assets/split
               :pixel-diff.math-js)
  :components ((:module "vendor"
                :components (("build-utils:js-file" "jquery-ui")
                             ("build-utils:js-file" "baguetteBox")
                             ("build-utils:js-file" "metisMenu")
                             ("build-utils:js-file" "select2")
                             ("build-utils:js-file" "moment")))
               ("build-utils:js-file" "jquery.timeago")
               ("build-utils:js-file" "default")
               ("build-utils:js-file" "js-stubs")
               ("build-utils:js-file" "common")
               ("build-utils:js-file" "image-canvas")
               ("build-utils:js-file" "runs")
               ("build-utils:js-file" "mask-editor")
               ("build-utils:js-file" "compare-branches")
               ("build-utils:js-file" "websocket-logs")
               ("build-utils:js-file" "acceptance")
               ("build-utils:js-file" "git-graph")
               ("build-utils:ps-file" "dummy")))


