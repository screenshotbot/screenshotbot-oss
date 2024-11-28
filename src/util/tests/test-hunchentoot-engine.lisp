;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/tests/test-hunchentoot-engine
  (:use #:cl
        #:fiveam)
  (:import-from #:hunchentoot
                #:define-easy-handler)
  (:import-from #:util/hunchentoot-engine
                #:hunchentoot-engine)
  (:import-from #:util/request
                #:http-request)
  (:import-from #:fiveam-matchers/core
                #:is-equal-to
                #:assert-that)
  (:import-from #:alexandria
                #:assoc-value))
(in-package :util/tests/test-hunchentoot-engine)


(util/fiveam:def-suite)

(define-easy-handler (test-handler :uri "/dummy/test-h-e" :acceptor-names '(foo)) ()
  (setf (hunchentoot:header-out :x-blah)  "22")
  "foobar")

(define-easy-handler (test-handler-2 :uri "/dummy/test-auth" :acceptor-names '(foo)) ()
  (multiple-value-bind (user pass) (hunchentoot:authorization)
   (format nil "~a/~a"
           user pass)))

(define-easy-handler (test-content-handler
                      :uri "/dummy/test-content-handler"
                      :acceptor-names '(foo))
    ()
  (format nil "~a" (hunchentoot:raw-post-data :force-text t)))

(def-fixture state ()
  (let* ((acceptor (make-instance 'hunchentoot:easy-acceptor
                                  :name 'foo))
         (engine (make-instance 'hunchentoot-engine
                                :acceptor acceptor)))
    (&body)))

(test simple-invocation ()
  (with-fixture state ()
    (assert-that
     (http-request
      "/dummy/test-h-e"
      :want-string t
      :engine engine)
     (is-equal-to "foobar"))))

(test code-and-headers ()
  (with-fixture state ()
    (multiple-value-bind (resp code headers)
        (http-request
         "/dummy/test-h-e"
         :want-string t
         :engine engine)
      (assert-that
       resp
       (is-equal-to "foobar"))
      (is (eql 200 code))
      (is (equal "22" (assoc-value headers :x-blah))))))

(test authentication
  (with-fixture state ()
    (let ((result (http-request
                   "/dummy/test-auth"
                   :want-string t
                   :engine engine
                   :basic-authorization '("foo" "bar"))))
      (is (equal "foo/bar" result)))))


(test want-stream
  (with-fixture state ()
    (let ((result (http-request
                   "/dummy/test-h-e"
                   :want-stream t
                   :engine engine)))
      (let ((sequence (make-array 6)))
        (read-sequence sequence result)
        (is (equalp
             "foobar"
             sequence))))))


(test want-stream-binary
  (with-fixture state ()
    (let ((result (http-request
                   "/dummy/test-h-e"
                   :want-stream t
                   :force-binary t
                   :engine engine)))
      (let ((sequence (make-array 6)))
        (read-sequence sequence result)
        (is (equalp
              #(102 111 111 98 97 114)
              sequence))))))

(test content-as-string
  (with-fixture state ()
    (let ((result (http-request
                   "/dummy/test-content-handler"
                   :content "foobar"
                   :content-type "application/text"
                   :want-string t
                   :engine engine)))
      (is (equal "foobar" result)))))

