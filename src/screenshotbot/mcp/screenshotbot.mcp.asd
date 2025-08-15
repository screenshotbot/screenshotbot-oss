(defsystem "screenshotbot.mcp"
  :description "Model Context Protocol integration for Screenshotbot"
  :author "Arnold Noronha <arnold@screenshotbot.io>"
  :license "Apache-2.0"
  :version "0.1.0"
  :serial t
  :depends-on (:screenshotbot
               :alexandria
               :cl-json
               :dexador
               :log4cl
               :str)
  :components ((:file "mcp")))

(defsystem "screenshotbot.mcp/tests"
  :description "Tests for screenshotbot.mcp"
  :depends-on (:screenshotbot.mcp
               :fiveam)
  :components ())

