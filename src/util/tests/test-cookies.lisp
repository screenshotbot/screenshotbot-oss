;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/tests/test-cookies
  (:use #:cl
        #:fiveam)
  (:import-from #:util/cookies
                #:set-cookie
                #:get-cookie
                #:cookies)
  (:local-nicknames (#:a #:alexandria)))
(in-package :util/tests/test-cookies)


(util/fiveam:def-suite)

(defclass fake-request ()
  ((cookies :initarg :cookies
            :reader cookies)))


(defmethod hunchentoot:cookies-in ((request fake-request))
  (cookies request))

(def-fixture state ()
  (let ((self (make-instance 'cookies
                             :request (make-instance 'fake-request
                                                     :cookies `(("zoid" . "berg")))
                             :reply (make-instance 'hunchentoot:reply)
                             :host "screenshotbot.io:2343"
                             :proto "https")))
    (&body)))

(test preconditions
  (with-fixture state ()
    (is-false (get-cookie self "foo"))
    (is (equal "berg" (get-cookie self "zoid")))))

(test write-cookie
  (with-fixture state ()
    (set-cookie self "foo" "bar")
    (pass)))
