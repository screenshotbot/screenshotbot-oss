(defpackage :screenshotbot.katalon-asdf
  (:use :cl :asdf))
(in-package :screenshotbot.katalon-asdf)

(defsystem :screenshotbot.katalon
    :serial t
    :depends-on (:util
                 :log4cl
                 :trivial-file-size)
    :components ((:file "deliver")))
