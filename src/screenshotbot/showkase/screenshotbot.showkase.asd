(defpackage :screenshotbot/showkase/screenshotbot.showkase.asd
  (:use #:cl
        #:asdf))
(in-package :screenshotbot/showkase/screenshotbot.showkase.asd)

(defsystem :screenshotbot.showkase
    :serial t
    :depends-on (:screenshotbot.sdk/library
                 :slynk
                 "slynk/arglists"
                 "slynk/fancy-inspector"
                 "slynk/package-fu"
                 "slynk/mrepl"
                 "slynk/trace-dialog"
                 "slynk/profiler"
                 "util/posix"
                 "slynk/stickers"
                 "slynk/indentation"
                 "slynk/retro"
                 "slynk-named-readtables"
                 "util.java"
                 :iterate)
    :components ((:file "lib")
                 (:file "main")))
