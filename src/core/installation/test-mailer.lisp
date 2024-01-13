;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :core/installation/test-mailer
  (:use :cl)
  (:import-from #:fiveam-matchers/core
                #:assert-that
                #:equal-to)
  (:import-from #:it.bese.fiveam
                #:is
                #:test)
  (:import-from #:screenshotbot/mailer
                #:background-mailer
                #:fix-email-list
                #:host
                #:local-smtp-mailer
                #:parse-from
                #:port
                #:send-mail))
(in-package :core/installation/test-mailer)

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

(defclass dummy-mailer ()
  ())

(defmethod send-mail ((mailer dummy-mailer) &rest args)
  :pass)

(test future-from-background-mailer
  (let ((mailer (make-instance 'background-mailer
                               :delegate (make-instance 'dummy-mailer))))
    (is
     (eql :pass
          (lparallel:force (send-mail mailer :from "foo" :to "bar"))))))


(test fix-email-list
  (is (equal "arnold@screenshotbot.io"
             (fix-email-list "Arnold Noronha <arnold@screenshotbot.io>")))
  (is (equal "arnold@screenshotbot.io"
             (fix-email-list "arnold@screenshotbot.io")))
  (is (equal (list "arnold@screenshotbot.io")
             (fix-email-list (list "Arnold Noronha <arnold@screenshotbot.io>"))))
    (is (equal (list "arnold@screenshotbot.io")
             (fix-email-list (list "arnold@screenshotbot.io"))))
  (is (equal nil
             (fix-email-list nil))))
