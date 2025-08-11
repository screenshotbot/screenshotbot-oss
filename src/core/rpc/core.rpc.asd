;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defsystem :core.rpc
  :serial t
  :depends-on (:core.api
               :server
               :util.threading
               (:feature (:and :linux :lispworks) :bknr.cluster)
               :encrypt)
  :components ((:file "rpc")))

(defsystem :core.rpc/tests
  :serial t
  :depends-on (:core.rpc
               :util/testing
               :util/fiveam)
  :components ((:file "test-rpc")))
