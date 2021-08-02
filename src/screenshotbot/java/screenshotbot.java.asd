;; Copyright 2018-Present Modern Interpreters Inc.
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defsystem :screenshotbot.java
  :author "Arnold Noronha <arnold@screenshotbot.io>"
  :license "Mozilla Public License, v 2.0"
  :serial t
  :depends-on (:str
               :pkg
               :closer-mop
               ;; We never want to load cl+j through quicklisp, always
               ;; call jvm:jvm-init instead.
               ;; :cl+j
               :named-readtables)
  :components (#+ (or ccl lispworks)
               (:file "java")
               (:file "reader")
               (:file "all")))

(defsystem :screenshotbot.java/tests
    :serial t
    :depends-on (:fiveam
                 :screenshotbot.java)
  :components (#+ (or ccl lispworks)
               (:file "test-binding")
               (:file "test-java")))
