;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :hunchentoot-extensions/test-random-port
  (:use #:cl
        #:fiveam)
  (:import-from #:hunchentoot-extensions/random-port
                #:acceptor-on-random-port))
(in-package :hunchentoot-extensions/test-random-port)

(util/fiveam:def-suite)

(def-fixture state ()
  (&body))

(defclass test-acc (acceptor-on-random-port
                    hunchentoot:easy-acceptor)
  ())

(hunchentoot:define-easy-handler (my-handler :uri "/hello" :acceptor-names '(foobar)) ()
  "OK")

;; TODO(T1368) This test is currently broken on our AWS ARM64 machine.
#-(and linux arm64)
(test happy-path-random-port
  (let ((acceptor (make-instance 'test-acc :name 'foobar)))
    (is-false (hunchentoot:started-p acceptor))
    (hunchentoot:start acceptor)
    (is-true (hunchentoot:started-p acceptor))
    (is
     (equal "OK"
            (dex:get (format nil "http://127.0.0.1:~a/hello" (hunchentoot:acceptor-port acceptor))
                     :read-timeout 3
                     :connect-timeout 3)))
    (hunchentoot:stop acceptor)
    (is-false (hunchentoot:started-p acceptor))
    (pass)))

(test stop-then-start
  (let ((acceptor (make-instance 'test-acc :name 'foobar)))
    (is-false (hunchentoot:started-p acceptor))
    (hunchentoot:start acceptor)
    (is-true (hunchentoot:started-p acceptor))
    (hunchentoot:stop acceptor)
    (is-false (hunchentoot:started-p acceptor))
    (hunchentoot:start acceptor)
    (is-true (hunchentoot:started-p acceptor))
    (is
     (equal "OK"
            (dex:get (format nil "http://127.0.0.1:~a/hello" (hunchentoot:acceptor-port acceptor))
                     :read-timeout 3
                     :connect-timeout 3)))
    (hunchentoot:stop acceptor)
    (is-false (hunchentoot:started-p acceptor))))

(test allow-shutting-down-twice
  (let ((acceptor (make-instance 'test-acc :name 'foobar)))
    (hunchentoot:start acceptor)
    (hunchentoot:stop acceptor)
    (is-false (hunchentoot:started-p acceptor))
    (finishes
      (hunchentoot:stop acceptor))
    (is-false (hunchentoot:started-p acceptor))))

(test allow-shutting-down-without-starting
  (let ((acceptor (make-instance 'test-acc :name 'foobar)))
    (finishes
     (hunchentoot:stop acceptor))
    (finishes
      (hunchentoot:stop acceptor))
    (is-false (hunchentoot:started-p acceptor))))
