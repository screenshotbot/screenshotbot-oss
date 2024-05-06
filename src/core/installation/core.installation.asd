;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defsystem #:core.installation
  :serial t
  :depends-on (#:util/html2text
               #:lparallel
               #:util/threading
               #:easy-macros
               #:cl-smtp)
  :components ((:file "installation")
               (:file "request")
               (:file "auth-provider")
               (:file "mailer")
               (:file "auth")))

(defsystem #:core.installation/tests
  :serial t
  :depends-on (#:core.installation
               #:util/fiveam)
  :components ((:file "test-mailer")
               (:file "test-request")))
