;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :hunchentoot-extensions/test-clos-dispatcher
  (:use #:cl
        #:fiveam)
  (:import-from #:fiveam-matchers/strings
                #:contains-string)
  (:import-from #:fiveam-matchers/core
                #:assert-that))
(in-package :hunchentoot-extensions/test-clos-dispatcher)

(util/fiveam:def-suite)

(named-readtables:in-readtable markup:syntax)

(defclass simple-acceptor (hex:clos-dispatcher
                           hunchentoot:acceptor)
  ())

(defclass another-acceptor (simple-acceptor)
  ())

(hex:def-clos-dispatch ((self simple-acceptor) "/hello") ()
  "hello world")

(defclass fake-request ()
  ((script-name :initarg :script-name
                :reader hunchentoot:script-name)))

(test simple-dispatch
 (is (equal "hello world"
            (hunchentoot:acceptor-dispatch-request
             (make-instance 'another-acceptor)
             (make-instance 'fake-request
                            :script-name "/hello")))))

(test simple-dispatch-on-parent
 (is (equal "hello world"
            (hunchentoot:acceptor-dispatch-request
             (make-instance 'simple-acceptor)
             (make-instance 'fake-request
                            :script-name "/hello")))))

(test failed-dispatch
  (is (equal nil
             (catch 'hunchentoot::handler-done
               (let ((hunchentoot:*reply* (make-instance 'hunchentoot:reply)))
                 (hunchentoot:acceptor-dispatch-request
                  (make-instance 'another-acceptor)
                  (make-instance 'fake-request
                                 :script-name "/hello2")))))))

(hex:def-clos-dispatch ((self simple-acceptor) "/hello3") ()
  "one")

(hex:def-clos-dispatch ((self another-acceptor) "/hello3") ()
  "two")

(test use-the-highest-precedence-possible
 (is (equal "two"
            (hunchentoot:acceptor-dispatch-request
             (make-instance 'another-acceptor)
             (make-instance 'fake-request
                            :script-name "/hello3")))))

(hex:def-clos-dispatch ((self simple-acceptor) "/markup") ()
  <html>
  hello
  </html>)

(test handles-markup
  (assert-that
   (hunchentoot:acceptor-dispatch-request
    (make-instance 'simple-acceptor)
    (make-instance 'fake-request
                   :script-name "/markup"))
   (contains-string "<html>")
   (contains-string "hello")))
