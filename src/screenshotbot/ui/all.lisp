(pkg:define-package :screenshotbot/ui
    (:use :cl
     :alexandria)
  (:use-reexport ./ui/confirmation-page
                 ./ui/simple-card-page
                 ./ui/core))
