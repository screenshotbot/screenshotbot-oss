;; Copyright 2018-Present Modern Interpreters Inc.
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defsystem "server/interrupts"
  :depends-on (:alexandria
               :bordeaux-threads)
  :serial t
  :components ((:file "interrupts")))

(defsystem "server"
  :depends-on ("cl-cli"
               "util.store"
               "cl-cron"
               "server/interrupts"
               #+ (or ccl lispworks)
               "jvm"
               #+lispworks
               "util/remote-debugging"
               "bordeaux-threads"
               "lparallel"
               "serapeum"
	       (:feature (:not (:or :mswindows :windows)) "control-socket")
               "easy-macros"
               (:feature (:not (:or :mswindows :windows)) "osicat")
               "util/health-check"
               "util/threading"
               "util/phabricator"
               "bknr.datastore"
               "hunchentoot-multi-acceptor")
  :serial t
  :components ((:file "interrupts")
               (:file "health-checks")
               (:file "control-socket")
               (:file "setup")))

(defsystem #:server/cli
  :depends-on (#:server
               #:clingon)
  :serial t
  :components ((:file "cli")))

;; For slynk support, load this before calling server:main. The reason
;; we separate this into a separate system is for support with SLIME,
;; which is needed if you're using Atom's SLIMA.
(defsystem "server/slynk"
  :depends-on ("server"
               "slynk"
               "slynk/arglists"
               "slynk/fancy-inspector"
               "slynk/package-fu"
               "slynk/mrepl"
               "slynk/trace-dialog"
               "slynk/profiler"
               "slynk/stickers"
               "slynk/indentation"
               "slynk/retro")
  :components ((:file "slynk-preparer")))

(defsystem :server/tests
  :depends-on (:server
               :cl-mock
               :util/fiveam
               :server/slynk)
  :components ((:file "test-server")
               (:file "test-slynk-preparer")))
