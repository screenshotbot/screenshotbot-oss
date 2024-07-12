;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :auth/test-auth
  (:use :cl
   :fiveam)
  (:import-from :util/testing
                :with-fake-request)
  (:import-from :auth
                #:cookie-name
                #:clean-session-values
                #:expiry-ts
                #:session-token
                #:session-domain
                #:session-key
                #:prop-key
                #:user-session-value
                #:generate-session-token
                #:csrf-token
   #+windows
   :read-windows-seed)
  (:import-from #:util/store
                #:with-test-store)
  (:import-from #:bknr.datastore
                #:class-instances)
  (:import-from #:fiveam-matchers
                #:is-string
                #:has-length
                #:assert-that)
  (:import-from #:core/installation/installation
                #:*installation*
                #:abstract-installation
                #:installation
                #:installation-domain)
  (:import-from #:fiveam-matchers/core
                #:has-typep
                #:is-not
                #:assert-that)
  (:import-from #:fiveam-matchers/lists
                #:contains
                #:has-item)
  (:import-from #:fiveam-matchers/strings
                #:is-string)
  (:export))
(in-package :auth/test-auth)

(util/fiveam:def-suite)

(def-fixture state ()
  (let ((*installation* (make-instance 'abstract-installation)))
   (cl-mock:with-mocks ()
     (with-test-store ()
       (with-fake-request ()
         (&body))))))

(test auth-simple-test
  (with-fixture state ()
    (auth:with-sessions ()
      (is-true (auth:current-session))
      (is (equal (auth:current-session)
                 (auth:current-session))))))

(defun last-user-session-value ()
  (car (last (class-instances 'user-session-value))))

(test simple-key-val
  (with-fixture state ()
    (auth:with-sessions ()
      (setf (auth:session-value :name)  33)
      (is (equal 33 (auth:session-value :name)))
      (setf (auth:session-value :name) 44)
      (is (equal 44 (auth:session-value :name))))))

(test set-key-val-with-expiry
  (with-fixture state ()
    (auth:with-sessions ()
      (setf (auth:session-value :name :expires-in 1000)  33)
      (is (equal 33 (auth:session-value :name)))
      (is (<
           (get-universal-time)
           (expiry-ts (last-user-session-value))
           (+ 1100 (get-universal-time)) ))
      (setf (auth:session-value :name :expires-in 500) 44)
      (is (equal 44 (auth:session-value :name)))
      (is (<
           (get-universal-time)
           (expiry-ts (last-user-session-value))
           (+ 600 (get-universal-time)) )))))

(test new-fields-are-set
  (with-fixture state ()
    (auth:with-sessions ()
      (setf (auth:session-value :name) 33)
      (is (eql :name (prop-key (last-user-session-value))))
      (is (equal "localhost" (session-domain (last-user-session-value))))
      (assert-that (session-token (last-user-session-value))
                   (is-string)
                   (has-length 32)))))

(test cleans-old-values
  (With-fixture state ()
    (auth:with-sessions ()
      (let ((ts (get-universal-time)))
        (setf (auth:session-value :name :expires-in 3000) "foobar")
        (is (equal "foobar" (auth:session-value :name)))
        (clean-session-values (+ ts 6000))
        (is (equal nil (auth:session-value :name)))))))

(test cleans-only-old-values
  (With-fixture state ()
    (auth:with-sessions ()
      (let ((ts (get-universal-time)))
        (setf (auth:session-value :name :expires-in 3000) "foobar")
        (setf (auth:session-value :bar :expires-in 9000) "foobar1")
        (setf (auth:session-value :car) "foobar2")
        (is (equal "foobar" (auth:session-value :name)))
        (clean-session-values (+ ts 6000))
        (is (equal nil (auth:session-value :name)))
        (is (equal "foobar1" (auth:session-value :bar)))
        (is (equal "foobar2" (auth:session-value :car)))))))

#+windows
(test read-windows-seed
  (is-true (read-windows-seed)))

(test csrf-token
  (with-fixture state ()
    (auth:with-sessions ()
     (cl-mock:answer (generate-session-token) "foobar"
       "bad")
      (is (equal "foobar" (auth:csrf-token)))
      (is (equal "foobar" (auth:csrf-token))))))


(test cookie-name
  (let ((*installation* (make-instance 'abstract-installation)))
    (is (equal "s2" (cookie-name))))
  (let ((*installation* (make-instance 'abstract-installation
                                       :domain "foo.example.com")))
    (is (equal "s3" (cookie-name)))))

(test generate-session-happey-path
  (finishes
    (generate-session-token)))

(test session-token-doesnt-get-called-if-theres-no-token
  (with-fixture state ()
    (auth:with-sessions ()
      (slot-makunbound
       (auth:current-session)
       'auth::session-key)

      (cl-mock:answer (auth::%session-token session)
        (error "session-token should not be read"))

      (is (eql nil (auth:session-value :hello-world))))))

(test simple-path-of-set-and-get-session-value
  (with-fixture state ()
    (flet ((cookies-out ()
             (loop for (key . val) in (hunchentoot:cookies-out*)
                   collect (list key val))))

     (auth:with-sessions ()
       (assert-that (hunchentoot:cookies-out*)
                    (contains))

       (setf (auth:session-value :foo) 22)

       (assert-that (cookies-out)
                    (contains
                     (contains "s2" (has-typep t))))

       (is (eql 22 (auth:session-value :foo)))))))
