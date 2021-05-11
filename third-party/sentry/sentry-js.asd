(defpackage :sentry-js-asdf
  (:use :cl :asdf))
(in-package :sentry-js-asdf)

(eval-when (:compile-toplevel :load-toplevel :execute)
   (unless (find-package 'build-utils)
     (asdf:operate 'asdf:load-op 'build-utils)))

(defsystem sentry-js
  :class build-utils:js-library
  :defsystem-depends-on (:build-utils)
  :components ((build-utils:js-file "bundle.min")
               (build-utils:js-file "config")))
