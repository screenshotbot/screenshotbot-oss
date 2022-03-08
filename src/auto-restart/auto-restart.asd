;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defsystem :auto-restart
  :author "Arnold Noronha <arnold@jipr.io>"
  :license  "Apache License, Version 2.0"
  :version "0.0.1"
  :description "automatically generate restart-cases for the most common use cases, and also use the restart for automatic retries "
  :serial t
  :depends-on (:iterate)
  :components ((:file "auto-restart"))
  :in-order-to ((test-op (test-op :auto-restart/tests))))

(defsystem :auto-restart/tests
  :serial t
  :depends-on (:auto-restart
               :fiveam)
  :components ((:file "test-auto-restart"))
  :perform (test-op (o c)
                    (unless
                        (symbol-call '#:fiveam '#:run!
                                      :test-auto-restart)
                      (error "Some tests were failed!"))))
