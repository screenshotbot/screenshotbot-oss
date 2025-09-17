(defpackage :bootstrap5-js-system
  (:use :cl
   :asdf))
(in-package :bootstrap5-js-system)


(defsystem :bootstrap5-js
  :class "build-utils:js-library"
  :depends-on (:jquery-js)
  :defsystem-depends-on (:build-utils)
  :components (("build-utils:js-file" "bootstrap.bundle")))
