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
  :components ((:file "mcp")
               ;; Each tool registers itself with DEF-TOOL at load time,
               ;; so these only need to load after mcp; nothing references
               ;; them by name.
               (:file "channels")
               (:file "reports")
               (:file "images")
               (:file "runs")))

(defsystem "screenshotbot.mcp/tests"
  :description "Tests for screenshotbot.mcp"
  ;; Serial because WITH-FIXTURE looks its fixture up at macroexpansion
  ;; time: test-util has to be loaded before anything that uses CALLER.
  :serial t
  :depends-on (:screenshotbot.mcp
               :fiveam
               :util/fiveam
               :util.testing
               :util.store
               :cl-mock
               :screenshotbot/testing-lib)
  :components ((:file "test-util")
               (:file "test-mcp")
               (:file "test-channels")
               (:file "test-reports")
               (:file "test-images")
               (:file "test-runs")))

