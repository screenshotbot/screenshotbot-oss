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

(defsystem "server/util"
    :depends-on (:bordeaux-threads)
    :components ((:file "util" :if-feature (:and :lispworks :linux)))
    :description "This system gets reloaded before we reload any complex code, which let's us put reloading logic in here. It also means we must keep the dependencies minimal")

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
                 (:feature (:and :lispworks (:not :mswindows)) "control-socket")
                 "easy-macros"
                 "util/health-check"
                 "util/threading"
                 "util/phabricator"
                 "hunchentoot-extensions"
                 "bknr.datastore"
                 "hunchentoot-multi-acceptor")
    :serial t
    :components ((:file "interrupts")
                 (:file "health-checks")
                 (:file "control-socket" :if-feature (:and :lispworks (:not :mswindows)))
                 (:file "setup")
                 (:file "util")))

(defsystem #:server/config
  :depends-on ()
  :components ((:file "config")))

(defsystem #:server/cli
  :depends-on (#:server
               #:server/config
               #:clingon)
  :serial t
  :components ((:file "eval" :if-feature :lispworks)
               (:file "cli")))

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
               "util/posix"
               "slynk/stickers"
               "slynk/indentation"
               "slynk/retro"
               "slynk-named-readtables")
  :components ((:file "slynk-preparer")))

(defsystem :server/tests
  :depends-on (:server
               :server/config
               :cl-mock
               :util/fiveam
               :fiveam-matchers
               :server/slynk)
  :components ((:file "test-server")
               (:file "test-config")
               (:file "test-slynk-preparer")))
