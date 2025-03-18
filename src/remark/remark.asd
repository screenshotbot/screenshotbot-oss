;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defsystem :remark
  :serial t
  :depends-on (:markup
               :str
               :nibble
               :util/events
               :iterate)
  :components ((:file "nodes")
               (:file "markdown")
               (:file "search")
               (:file "render")
               (:file "all")))

(defsystem :remark/js
  :serial t
  :class "build-utils:js-system"
  :defsystem-depends-on (:build-utils)
  :depends-on (:jquery-js)
  :components ((:module "js"
                :components (("build-utils:js-file" "remark")))))

(defsystem :remark/tests
  :serial t
  :depends-on (:remark
               :fiveam)
  :components ((:file "test-nodes")
               (:file "test-search")
               (:file "test-markdown")))
