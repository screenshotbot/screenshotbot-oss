(defpackage :bootstrap-js-system
  (:use :cl
   :asdf))
(in-package :bootstrap-js-system)

(defsystem :popper-js
  :class "build-utils:js-library"
  :depends-on (:jquery-js)
  :defsystem-depends-on (:build-utils)
  :components (("build-utils:js-file" "popper")))
