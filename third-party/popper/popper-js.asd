(defpackage :bootstrap-js-system
  (:use :cl
   :asdf))
(in-package :bootstrap-js-system)

(eval-when (:compile-toplevel :load-toplevel :execute)
   (unless (find-package 'build-utils)
     (asdf:operate 'asdf:load-op 'build-utils)
     (use-package :build-utils)))

(defsystem :popper-js
  :class build-utils:js-library
  :depends-on (:jquery-js)
  :components ((build-utils:js-file "popper")))
