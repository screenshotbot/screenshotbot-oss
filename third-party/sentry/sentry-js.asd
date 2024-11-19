(defpackage :sentry-js-asdf
  (:use :cl :asdf))
(in-package :sentry-js-asdf)

(defsystem sentry-js
  :class "build-utils:js-library"
  :defsystem-depends-on (:build-utils)
  :components (("build-utils:js-file" "bundle.min")
               #-screenshotbot-oss
               ("build-utils:js-file" "config")))
