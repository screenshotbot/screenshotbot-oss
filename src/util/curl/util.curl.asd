;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defsystem :util.curl
  :depends-on (:util/request
               :drakma
               :cl-base64
               :chunga
               :flexi-streams
               :puri
               :quri
               :alexandria
               :bordeaux-threads
               :log4cl)
  :defsystem-depends-on (:trivial-features)
  :serial t
  :components ((:file "curl-engine")
               (:file "curl-request" :if-feature (:and :lispworks (:not (:or :mswindows :win32))))))

(defsystem :util.curl/tests
  :depends-on (:util.curl
               :hunchentoot
               :fiveam
               :util.testing
               :util/fiveam)
  :serial t
  :components ((:file "test-curl-request")))
