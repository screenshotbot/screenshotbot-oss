(defpackage :screenshotbot-system.js-assets
  (:use :cl :asdf))
(in-package :screenshotbot-system.js-assets)

(eval-when (:compile-toplevel :load-toplevel :execute)
   (unless (find-package 'build-utils)
     (asdf:operate 'asdf:load-op 'build-utils)
     (use-package :build-utils)))

(defsystem :jquery-js
  :class build-utils:js-library
  :defsystem-depends-on (:build-utils)
  :components ((:module "vendor"
                :components ((build-utils:js-file "jquery-3.5.1")))))
