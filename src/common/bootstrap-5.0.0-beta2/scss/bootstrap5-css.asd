(defpackage :bootstrap5-css-assets
  (:use :cl :asdf))
(in-package :bootstrap5-css-assets)

(defsystem :bootstrap5-css
  :class "build-utils:css-library"
  :defsystem-depends-on (:build-utils)
  :import-path #P"bootstrap/scss/"
  ;; (build-utils/css-package::get-css-component "/home/arnold/builds/web/src/common/bootstrap-5.0.0-beta2/scss/")
  :components (("BUILD-UTILS/CSS-PACKAGE:SCSS-FILE" "_progress")
               ("BUILD-UTILS/CSS-PACKAGE:SCSS-FILE" "_nav")
               ("BUILD-UTILS/CSS-PACKAGE:SCSS-FILE" "_images")
               ("BUILD-UTILS/CSS-PACKAGE:SCSS-FILE" "_card")
               (:MODULE
                "mixins"
                :COMPONENTS
                (("BUILD-UTILS/CSS-PACKAGE:SCSS-FILE" "_caret")
                 ("BUILD-UTILS/CSS-PACKAGE:SCSS-FILE" "_border-radius")
                 ("BUILD-UTILS/CSS-PACKAGE:SCSS-FILE" "_table-variants")
                 ("BUILD-UTILS/CSS-PACKAGE:SCSS-FILE" "_visually-hidden")
                 ("BUILD-UTILS/CSS-PACKAGE:SCSS-FILE" "_reset-text")
                 ("BUILD-UTILS/CSS-PACKAGE:SCSS-FILE" "_clearfix")
                 ("BUILD-UTILS/CSS-PACKAGE:SCSS-FILE" "_resize")
                 ("BUILD-UTILS/CSS-PACKAGE:SCSS-FILE" "_pagination")
                 ("BUILD-UTILS/CSS-PACKAGE:SCSS-FILE" "_list-group")
                 ("BUILD-UTILS/CSS-PACKAGE:SCSS-FILE" "_image")
                 ("BUILD-UTILS/CSS-PACKAGE:SCSS-FILE" "_lists")
                 ("BUILD-UTILS/CSS-PACKAGE:SCSS-FILE" "_buttons")
                 ("BUILD-UTILS/CSS-PACKAGE:SCSS-FILE" "_transition")
                 ("BUILD-UTILS/CSS-PACKAGE:SCSS-FILE" "_gradients")
                 ("BUILD-UTILS/CSS-PACKAGE:SCSS-FILE" "_grid")
                 ("BUILD-UTILS/CSS-PACKAGE:SCSS-FILE" "_breakpoints")
                 ("BUILD-UTILS/CSS-PACKAGE:SCSS-FILE" "_alert")
                 ("BUILD-UTILS/CSS-PACKAGE:SCSS-FILE" "_utilities")
                 ("BUILD-UTILS/CSS-PACKAGE:SCSS-FILE" "_text-truncate")
                 ("BUILD-UTILS/CSS-PACKAGE:SCSS-FILE" "_deprecate")
                 ("BUILD-UTILS/CSS-PACKAGE:SCSS-FILE" "_forms")
                 ("BUILD-UTILS/CSS-PACKAGE:SCSS-FILE" "_box-shadow")
                 ("BUILD-UTILS/CSS-PACKAGE:SCSS-FILE" "_container")))
               ("BUILD-UTILS/CSS-PACKAGE:SCSS-FILE" "bootstrap-utilities")
               ("BUILD-UTILS/CSS-PACKAGE:SCSS-FILE" "_reboot")
               ("BUILD-UTILS/CSS-PACKAGE:SCSS-FILE" "bootstrap-reboot")
               ("BUILD-UTILS/CSS-PACKAGE:SCSS-FILE" "bootstrap")
               ("BUILD-UTILS/CSS-PACKAGE:SCSS-FILE" "_navbar")
               ("BUILD-UTILS/CSS-PACKAGE:SCSS-FILE" "_pagination")
               ("BUILD-UTILS/CSS-PACKAGE:SCSS-FILE" "_accordion")
               ("BUILD-UTILS/CSS-PACKAGE:SCSS-FILE" "_list-group")
               ("BUILD-UTILS/CSS-PACKAGE:SCSS-FILE" "_containers")
               ("BUILD-UTILS/CSS-PACKAGE:SCSS-FILE" "_variables")
               ("BUILD-UTILS/CSS-PACKAGE:SCSS-FILE" "_type")
               ("BUILD-UTILS/CSS-PACKAGE:SCSS-FILE" "_mixins")
               ("BUILD-UTILS/CSS-PACKAGE:SCSS-FILE" "_popover")
               ("BUILD-UTILS/CSS-PACKAGE:SCSS-FILE" "_buttons")
               ("BUILD-UTILS/CSS-PACKAGE:SCSS-FILE" "_modal")
               ("BUILD-UTILS/CSS-PACKAGE:SCSS-FILE" "_transitions")
               (:MODULE
                "utilities"
                :COMPONENTS
                (("BUILD-UTILS/CSS-PACKAGE:SCSS-FILE" "_api")))
               ("BUILD-UTILS/CSS-PACKAGE:SCSS-FILE" "_carousel")
               (:MODULE
                "forms"
                :COMPONENTS
                (("BUILD-UTILS/CSS-PACKAGE:SCSS-FILE" "_form-select")
                 ("BUILD-UTILS/CSS-PACKAGE:SCSS-FILE" "_form-check")
                 ("BUILD-UTILS/CSS-PACKAGE:SCSS-FILE" "_form-text")
                 ("BUILD-UTILS/CSS-PACKAGE:SCSS-FILE" "_form-control")
                 ("BUILD-UTILS/CSS-PACKAGE:SCSS-FILE" "_input-group")
                 ("BUILD-UTILS/CSS-PACKAGE:SCSS-FILE" "_form-range")
                 ("BUILD-UTILS/CSS-PACKAGE:SCSS-FILE" "_validation")
                 ("BUILD-UTILS/CSS-PACKAGE:SCSS-FILE" "_labels")
                 ("BUILD-UTILS/CSS-PACKAGE:SCSS-FILE" "_floating-labels")))
               ("BUILD-UTILS/CSS-PACKAGE:SCSS-FILE" "_tables")
               ("BUILD-UTILS/CSS-PACKAGE:SCSS-FILE" "_dropdown")
               ("BUILD-UTILS/CSS-PACKAGE:SCSS-FILE" "_grid")
               ("BUILD-UTILS/CSS-PACKAGE:SCSS-FILE" "_toasts")
               ("BUILD-UTILS/CSS-PACKAGE:SCSS-FILE" "_close")
               ("BUILD-UTILS/CSS-PACKAGE:SCSS-FILE" "_functions")
               ("BUILD-UTILS/CSS-PACKAGE:SCSS-FILE" "_spinners")
               ("BUILD-UTILS/CSS-PACKAGE:SCSS-FILE" "_tooltip")
               ("BUILD-UTILS/CSS-PACKAGE:SCSS-FILE" "_button-group")
               ("BUILD-UTILS/CSS-PACKAGE:SCSS-FILE" "_alert")
               ("BUILD-UTILS/CSS-PACKAGE:SCSS-FILE" "_breadcrumb")
               ("BUILD-UTILS/CSS-PACKAGE:SCSS-FILE" "_utilities")
               ("BUILD-UTILS/CSS-PACKAGE:SCSS-FILE" "_helpers")
               (:MODULE
                "helpers"
                :COMPONENTS
                (("BUILD-UTILS/CSS-PACKAGE:SCSS-FILE" "_visually-hidden")
                 ("BUILD-UTILS/CSS-PACKAGE:SCSS-FILE" "_stretched-link")
                 ("BUILD-UTILS/CSS-PACKAGE:SCSS-FILE" "_position")
                 ("BUILD-UTILS/CSS-PACKAGE:SCSS-FILE" "_clearfix")
                 ("BUILD-UTILS/CSS-PACKAGE:SCSS-FILE" "_colored-links")
                 ("BUILD-UTILS/CSS-PACKAGE:SCSS-FILE" "_text-truncation")
                 ("BUILD-UTILS/CSS-PACKAGE:SCSS-FILE" "_ratio")))
               (:MODULE
                "vendor"
                :COMPONENTS
                (("BUILD-UTILS/CSS-PACKAGE:SCSS-FILE" "_rfs")))
               ("BUILD-UTILS/CSS-PACKAGE:SCSS-FILE" "_badge")
               ("BUILD-UTILS/CSS-PACKAGE:SCSS-FILE" "bootstrap-grid")
               ("BUILD-UTILS/CSS-PACKAGE:SCSS-FILE" "_root")
               ("BUILD-UTILS/CSS-PACKAGE:SCSS-FILE" "_forms")))
