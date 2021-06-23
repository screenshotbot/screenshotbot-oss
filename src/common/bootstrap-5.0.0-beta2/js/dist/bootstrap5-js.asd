(defpackage :bootstrap5-js-system
  (:use :cl
   :asdf))
(in-package :bootstrap5-js-system)

(eval-when (:compile-toplevel :load-toplevel :execute)
   (unless (find-package 'build-utils)
     (asdf:operate 'asdf:load-op 'build-utils)
     (use-package :build-utils)))


(defsystem :bootstrap5-js/dom
  :class build-utils:js-library
  :defsystem-depends-on (:build-utils)
  :components ((:module "dom"
                :components ((build-utils:js-file "data")
                             (build-utils:js-file "selector-engine")
                             (build-utils:js-file "manipulator")
                             (build-utils:js-file "event-handler")))))

(defsystem :bootstrap5-js/base-component
  :class build-utils:js-library
  :defsystem-depends-on (:build-utils)
  :depends-on (:bootstrap5-js/dom)
  :components ((build-utils:js-file "base-component")))


(defsystem :bootstrap5-js/collapse
    :class build-utils:js-library
    :defsystem-depends-on (:build-utils)
  :depends-on(:bootstrap5-js/base-component
              :popper-js)
  :components ((build-utils:js-file "collapse")))

(defsystem :bootstrap5-js
  :class build-utils:js-library
  :depends-on (:bootstrap5-js/collapse)
  :defsystem-depends-on (:build-utils)
  :components ((build-utils:js-file "alert")
               (build-utils:js-file "button")
               (build-utils:js-file "carousel")
               (build-utils:js-file "modal")
               (build-utils:js-file "dropdown")
               (build-utils:js-file "tooltip")
               (build-utils:js-file "popover" :depends-on ("tooltip"))
               (build-utils:js-file "scrollspy")
               (build-utils:js-file "tab")
               (build-utils:js-file "toast")))
