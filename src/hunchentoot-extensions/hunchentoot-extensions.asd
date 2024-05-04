;; Copyright 2018-Present Modern Interpreters Inc.
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defsystem :hunchentoot-extensions
  :serial t
  :depends-on (:hunchentoot
               :str
               :markup
               :auto-restart
               :quri
               :easy-macros
               :closer-mop
               :do-urlencode
               :util/threading
               :log4cl)
  :components ((:file "package")
               (:file "url")
               (:file "existing-socket")
               (:file "random-port")
               (:file "acceptor-with-plugins")
               (:file "clos-dispatcher")
               (:file "better-easy-handler")
               (:file "postdata")
               (:file "async")
               (:file "forward")
               (:file "asdf-acceptor")
               (:file "webp")))

(defsystem :hunchentoot-extensions/tests
  :serial t
  :depends-on (:hunchentoot-extensions
               :lparallel
               :util/request
               :util/testing
               :util/fiveam
               :dexador
               :fiveam-matchers
               :fiveam)
  :components ((:file "test-acceptor-with-plugins")
               (:file "test-random-port")
               (:file "test-clos-dispatcher")
               (:file "test-better-easy-handler")
               (:file "test-url")
               (:file "test-async")))
