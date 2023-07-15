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

(test happy-path
  (let ((acceptor (make-instance 'test-acc)))
    (hunchentoot:start acceptor)
    (hunchentoot:stop acceptor)
    (pass)))
