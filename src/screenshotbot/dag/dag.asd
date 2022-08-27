;; Copyright 2018-Present Modern Interpreters Inc.
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defsystem :dag
  :serial t
  :depends-on (:graph
               :pkg
               :ironclad)
  :components ((:file "package")
               (:file "dag")))

(defsystem :dag/tests
  :serial t
  :depends-on (:dag
               :fiveam
               :util/fiveam)
  :components ((:file "test-dag")))
