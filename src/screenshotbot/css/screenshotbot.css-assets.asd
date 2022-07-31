(defpackage :screenshotbot-system.css-assets
  (:use :cl :asdf))
(in-package :screenshotbot-system.css-assets)

(defsystem screenshotbot.css-assets/library
  :class "BUILD-UTILS:CSS-LIBRARY"
  :defsystem-depends-on (:build-utils)
  :depends-on (:bootstrap5-css)
  :components (("BUILD-UTILS:SCSS-FILE" "sidebar")
               ("BUILD-UTILS:SCSS-FILE" "material-icons")
               ("BUILD-UTILS:CSS-FILE" "bootstrap-icons")
               ("BUILD-UTILS:SCSS-FILE" "split")
               (:MODULE "vendor"
                :COMPONENTS (("BUILD-UTILS:CSS-FILE" "baguetteBox")
                             ("BUILD-UTILS::CSS-FILE" "jquery-jvectormap-1.2.2")
                             ("BUILD-UTILS::CSS-FILE" "dataTables.bootstrap4")
                             ("BUILD-UTILS::CSS-FILE" "select.bootstrap4")
                             ("BUILD-UTILS:SCSS-FILE" "_jquery.bootstrap-touchspin.min")
                             ("BUILD-UTILS:SCSS-FILE" "_daterangepicker")
                             ("BUILD-UTILS::CSS-FILE" "frappe-gantt")
                             ("BUILD-UTILS:SCSS-FILE" "_select2.min")
                             ("BUILD-UTILS::CSS-FILE" "responsive.bootstrap4") ("BUILD-UTILS::CSS-FILE" "summernote-bs4") ("BUILD-UTILS:SCSS-FILE" "_bootstrap-datepicker.min") ("BUILD-UTILS:SCSS-FILE" "_bootstrap-timepicker.min") ("BUILD-UTILS::CSS-FILE" "britecharts.min") ("BUILD-UTILS:SCSS-FILE" "_jquery.toast.min") ("BUILD-UTILS::CSS-FILE" "simplemde.min") ("BUILD-UTILS::CSS-FILE" "fullcalendar.min") ("BUILD-UTILS::CSS-FILE" "buttons.bootstrap4")))  ("BUILD-UTILS:SCSS-FILE" "headroom-custom")
 ("BUILD-UTILS:SCSS-FILE" "default") ("BUILD-UTILS:SCSS-FILE" "variables")
                ("BUILD-UTILS:SCSS-FILE" "avatar") ("BUILD-UTILS:SCSS-FILE" "breakpoints")
               ("BUILD-UTILS:SCSS-FILE" "auth")))

;;(build-utils::get-css-component #P "~/builds/web/screenshotbot/static/assets/css/")

(defsystem screenshotbot.css-assets
  :class build-utils:css-system
  :defsystem-depends-on (:build-utils)
  :depends-on (:screenshotbot.css-assets/library)
  :components (("BUILD-UTILS:SCSS-FILE" "default")))

(defsystem screenshotbot.css-assets/doks
  :class build-utils:css-system
  :defsystem-depends-on (:build-utils)
  :depends-on (:bootstrap-css
               :doks-css
               :screenshotbot.css-assets/library)
  :components (("BUILD-UTILS:SCSS-FILE" "doks-default")))
