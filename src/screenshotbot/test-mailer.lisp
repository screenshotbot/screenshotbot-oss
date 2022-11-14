;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/test-mailer
    (:use #:cl
          #:fiveam
          #:alexandria)
  (:import-from #:screenshotbot/mailer
                #:parse-from
                #:port
                #:host
                #:local-smtp-mailer)
  (:import-from #:fiveam-matchers
                #:equal-to
                #:assert-that
                #:is-equal-to))
(in-package :screenshotbot/test-mailer)

(util/fiveam:def-suite)

(test local-smtp-mailer
  (let ((mailer (make-instance 'local-smtp-mailer)))
    (assert-that (host mailer)
                 (equal-to "localhost"))
    (assert-that (port mailer)
                 (equal-to 25))))

(test parse-from
  (flet ((parse (x y)
           (multiple-value-list (parse-from x y))))
    (is (equal (list "foo@goo.com" "Foo Goo")
               (parse "foo@goo.com" "Foo Goo")))
    (is (equal (list "foo@goo.com" nil)
               (parse "foo@goo.com" nil)))
    (is (equal (list "foo@goo.com" "Foo Goo")
               (parse "Foo Goo<foo@goo.com>" nil)))
    (is (equal (list "foo@goo.com" "Foo Goo")
               (parse "Foo Goo <foo@goo.com>" nil)))))
