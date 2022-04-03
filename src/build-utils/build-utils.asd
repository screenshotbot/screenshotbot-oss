;; Copyright 2018-Present Modern Interpreters Inc.
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defsystem :build-utils/closure-compiler
  :class "BUILD-UTILS/REMOTE-FILE:REMOTE-FILE"
  :defsystem-depends-on (:build-utils.remote-file)
  :remote-file-type "jar"
  :version "20220301"
  :url "https://repo1.maven.org/maven2/com/google/javascript/closure-compiler/v20220301/closure-compiler-v20220301.jar")

(defsystem :build-utils
  :serial t
  :depends-on (:tmpdir
               :build-utils.remote-file
               :build-utils/closure-compiler)
  :components ((:file "wild-module")
               (:file "common")
               (:file "jar-file")
               (:file "js-package")
               (:file "css-package")
               (:file "intellij-plugin")
               (:file "all")))
