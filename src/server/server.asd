;; Copyright 2018-Present Modern Interpreters Inc.
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defsystem "server"
  :depends-on ("cl-cli"
               "util"
               "cl-cron"
               "jvm"
               "trivial-shell"
               "bordeaux-threads"
               "bknr.datastore"
               "hunchentoot-multi-acceptor")
  :serial t
  :components ((:file "setup")))

(defsystem :server/tests
    :depends-on (:server))
