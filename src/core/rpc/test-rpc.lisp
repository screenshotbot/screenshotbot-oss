;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :core/rpc/test-rpc
  (:use #:cl
        #:fiveam)
  (:import-from #:util/store/store
                #:with-test-store)
  (:import-from #:core/rpc/rpc
                #:rpc-auth-id
                #:rpc-authentication-failed
                #:authenticate-rpc-request)
  (:import-from #:util/testing
                #:with-fake-request)
  (:import-from #:bknr.datastore
                #:store-object-id))
(in-package :core/rpc/test-rpc)

(util/fiveam:def-suite)

(def-fixture state ()
  (with-test-store ()
    (with-fake-request ()
      (&body))))

(defun make-authorization (key pass)
  `((:authorization . ,(format nil "Basic ~a"
                               (base64:string-to-base64-string (format nil "~a:~a" key pass))))))

(test authenticate-rpc ()
  (with-fixture state ()
    (signals rpc-authentication-failed
      (authenticate-rpc-request (make-instance 'hunchentoot:request
                                               :uri "foo"
                                               :headers-in (make-authorization "foo" "bar"))))
    (let ((auth-id (make-instance 'rpc-auth-id
                                  :secret "bleh")))

     (finishes
       (authenticate-rpc-request
        (make-instance 'hunchentoot:request
                       :uri "foo"
                       :headers-in (make-authorization (store-object-id auth-id)
                                                       "bleh"))))
      (signals rpc-authentication-failed
        (authenticate-rpc-request
         (make-instance 'hunchentoot:request
                        :uri "foo"
                        :headers-in (make-authorization (store-object-id auth-id)
                                                        "car")))))))
