;; Copyright 2018-Present Modern Interpreters Inc.
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defsystem :build-utils/core
  :serial t
  :depends-on (:tmpdir
               :str
               :cl-store
               :alexandria)
  :components ((:file "wild-module")
               (:file "common")
               (:file "remote-file")
               (:file "jar-file")))

(defsystem :build-utils/js-package
  :serial t
  :depends-on (:build-utils/core
               :parenscript)
  :components ((:file "js-package")
               (:file "css-package")))

(defsystem :build-utils
  :serial t
  :depends-on (:build-utils/core
               :build-utils/js-package)
  :components ((:file "all")))

(defsystem :build-utils/deliver-script
  :serial t
  :depends-on (:tmpdir
               :alexandria
               :trivial-features
               :str)
  :components ((:file "deliver-script")))
