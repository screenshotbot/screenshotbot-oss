;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :encrypt/test-hmac
  (:use #:cl
        #:fiveam)
  (:import-from #:util/store/store
                #:with-test-store)
  (:import-from #:encrypt/hmac
                #:verify-hmac
                #:sign-hmac
                #:hmac-key)
  (:import-from #:fiveam-matchers/core
                #:assert-that)
  (:import-from #:fiveam-matchers/has-length
                #:has-length))
(in-package :encrypt/test-hmac)

(util/fiveam:def-suite)

(def-fixture state ()
  (with-test-store ()
    (&body)))

(test hmac-key-happy-path
  (with-fixture state ()
    (finishes (hmac-key))
    (finishes (hmac-key))))

(test digest-string
  (with-fixture state ()
    (assert-that (sign-hmac (flex:string-to-octets "foobar"))
                 (has-length 32))))

(test digest-string
  (with-fixture state ()
    (assert-that (sign-hmac "foobar")
                 (has-length 32))))


(test verification
  (with-fixture state ()
    (let ((signature (sign-hmac "foobar"))
          (sign2 (sign-hmac "bar")))
      (is-true (verify-hmac "foobar" signature))
      (is-false (verify-hmac "foobar" sign2)))))
