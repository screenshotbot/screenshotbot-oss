(defsystem :screenshotbot.localhost
  :serial t
  :depends-on (:screenshotbot
               #-screenshotbot-oss
               :screenshotbot.pro.css/extended-dashboard
               :clingon)
  :components ((:file "run")
               (:file "init")))
