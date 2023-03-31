(defsystem :screenshotbot.localhost
  :serial t
  :depends-on (:screenshotbot
               :clingon)
  :components ((:file "run")
               (:file "init")))
