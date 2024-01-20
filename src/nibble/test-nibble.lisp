;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :nibble/test-nibble
  (:use #:cl
        #:fiveam)
  (:import-from #:nibble
                #:nibble-full-url
                #:expired-nibble
                #:make-id
                #:nibble-url
                #:nibble-current-user
                #:defnibble
                #:nibble
                #:render-nibble)
  (:import-from #:fiveam-matchers/core
                #:assert-that)
  (:import-from #:fiveam-matchers/strings
                #:starts-with
                #:contains-string)
  (:local-nicknames (#:a #:alexandria)))
(in-package :nibble/test-nibble)

(def-suite* :nibble)
(def-suite* :nibble/test-nibble :in :nibble)

(defclass fake-acceptor (nibble:nibble-acceptor-mixin)
  ())

(defmethod nibble-current-user ((acceptor fake-acceptor))
  :dummy-user)

(def-fixture state ()
  (let ((hunchentoot:*acceptor* (make-instance 'fake-acceptor)))
   (&body)))

(test preconditions
  (with-fixture state ()
   (is (equal "foobar"
              (render-nibble
               hunchentoot:*acceptor*
               (nibble ()
                 "foobar"))))))


(defnibble foo ()
  "foobar2")

(test named-nibble
  (with-fixture state ()
    (is (equal "foobar2"
               (render-nibble
                hunchentoot:*acceptor*
                (nibble foo))))))

(defnibble foo-with-args (name)
  (format nil "hello ~a" name))

(test named-nibble-with-args
  (with-fixture state ()
    (util/testing:with-fake-request (:script-name "/?name=arnold")
      (auth:with-sessions ()
       (is (equal "hello arnold"
                  (render-nibble
                   hunchentoot:*acceptor*
                   (nibble foo-with-args))))))))

(test with-name-renders-url
  (with-fixture state ()
    (util/testing:with-fake-request ()
      (auth:with-sessions ()
       (let ((nibble (nibble (:name :foobar)
                       "")))

         (is (str:ends-with-p "&_n=foobar"
                              (nibble-url  nibble)))
         (assert-that (nibble-url nibble)
                      (starts-with "/n/")))))))

(test render-nibble
  (with-fixture state ()
    (is (str:containsp "NIBBLE"
                       (format nil "~a" (nibble () "dummy"))))))

(test make-id-happy-path
  (with-fixture state ()
    (is (> (make-id (get-universal-time)) 10000000))))

(test expired-nibble
  (with-fixture state ()
    (util/testing:with-fake-request ()
      (render-nibble hunchentoot:*acceptor* "342343243343432232213123123")
      (is (eql 410 (hunchentoot:return-code hunchentoot:*reply*)))
      (is (equal "1" (hunchentoot:header-out :x-expired-nibble))))))

(test expired-nibble-signals-warnings
  (with-fixture state ()
    (util/testing:with-fake-request ()
      (signals expired-nibble
       (render-nibble hunchentoot:*acceptor* "342343243343432232213123123")))))


(test nibble-full-url-happy-path
  (with-fixture state ()
    (util/testing:with-fake-request ()
      (auth:with-sessions ()
        (nibble-full-url (nibble ()))))))

(test rendering-a-nibble-that-already-sent-a-response
  (with-fixture state ()
    (util/testing:with-fake-request ()
      (auth:with-sessions ()
        (finishes
          (render-nibble
           hunchentoot:*acceptor*
           (nibble ()
             ;; We might have already returned a response by this
             ;; point.
             nil)))))))

(test expired-nibble-format
  (assert-that (format nil "~a"
                       (make-condition 'expired-nibble
                                       :name "foo"
                                       :src "bleh"))
               (contains-string "foo")
               (contains-string "bleh")))
