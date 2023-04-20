;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defsystem :gatekeeper
  :serial t
  :depends-on (:bknr.datastore
               :util.store)
  :components ((:file "gatekeeper")))

(defsystem :gatekeeper/tests
  :serial t
  :depends-on (:util/fiveam
               :gatekeeper))
