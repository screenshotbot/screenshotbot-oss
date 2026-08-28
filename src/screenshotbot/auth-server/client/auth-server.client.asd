;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defsystem :auth-server.client
  :description "OAuth 2.0 client for the Screenshotbot authorization server.

Deliberately standalone: it depends on nothing from :screenshotbot, so it
can be linked into a CLI binary without dragging the server in."
  :author "Arnold Noronha <arnold@screenshotbot.io>"
  :license "Mozilla Public License, v 2.0"
  :serial t
  :depends-on (:alexandria
               :cl-base64
               :cl-json
               :drakma
               :easy-macros
               :flexi-streams
               :hunchentoot-extensions
               :ironclad
               :log4cl
               :quri
               :secure-random
               :str)
  :components ((:file "conditions")
               (:file "http")
               (:file "pkce")
               (:file "discovery")
               (:file "token-store")
               (:file "loopback")
               (:file "flows")
               (:file "client")))

(defsystem :auth-server.client/cli
  :description "The clingon command line front end to :auth-server.client"
  :serial t
  :depends-on (:auth-server.client
               :clingon)
  :components ((:file "cli")))

(defsystem :auth-server.client/tests
  :serial t
  :depends-on (:auth-server.client
               :cl-mock
               :fiveam
               :fiveam-matchers
               :tmpdir
               :util/fiveam)
  :components ((:file "test-pkce")
               (:file "test-token-store")
               (:file "test-discovery")
               (:file "test-flows")
               (:file "test-client")))
