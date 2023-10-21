(defpackage :screenshotbot/showkase/screenshotbot.showkase.asd
  (:use #:cl
        #:asdf))
(in-package :screenshotbot/showkase/screenshotbot.showkase.asd)

(defsystem :screenshotbot.showkase
  :depends-on (:screenshotbot.sdk/library
               :slynk)
  :components ((:file "main")))
