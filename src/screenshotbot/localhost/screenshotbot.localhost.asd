(defsystem :screenshotbot.localhost
  :serial t
  :depends-on (:screenshotbot
               :clingon)
  :components ((:file "init")))
