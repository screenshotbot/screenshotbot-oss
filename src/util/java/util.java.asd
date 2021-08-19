;; Copyright 2018-Present Modern Interpreters Inc.
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defsystem :util.java
  :author "Arnold Noronha <arnold@screenshotbot.io>"
  :license "Mozilla Public License, v 2.0"
  :serial t
  :depends-on (:str
               :pkg
               :iterate
               :closer-mop
               ;; We never want to load cl+j through quicklisp, always
               ;; call jvm:jvm-init instead.
               ;; :cl+j
               :named-readtables)
  :components (#+ (or ccl lispworks)
               (:file "java")
               (:file "reader")
               #+ (or ccl lispworks)
               (:file "binding")
               (:file "iterate")
               (:file "all")))

(defsystem :util.java/tests
    :serial t
  :depends-on (:fiveam
               :fiveam-matchers
               :util.java)
  :components (#+ (or ccl lispworks)
               (:file "test-binding")
               #+ (or ccl lispworks)
               (:file "test-java")
               #+ (or ccl lispworks)
               (:file "test-iterate")))
