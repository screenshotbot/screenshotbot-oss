;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defsystem :core.ui
  :serial t
  :depends-on (:markup)
  :components ((:file "template")
               (:file "simple-card-page")
               (:file "mdi")
               (:file "paginated")))

(defsystem :core.ui/tests
  :serial t
  :depends-on (:core.ui
               :util/fiveam)
  :components ((:file "test-paginated")))
