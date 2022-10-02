;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :hunchentoot-extensions/test-async
  (:use #:cl
        #:fiveam)
  (:shadow :get)
  (:import-from #:util/testing
                #:with-global-binding
                #:with-local-acceptor
                #:test-acceptor
                #:with-fake-request)
  (:import-from #:hunchentoot
                #:content-type*
                #:define-easy-handler)
  (:import-from #:hunchentoot-extensions
                #:better-easy-handler)
  (:import-from #:util/request
                #:http-request)
  (:import-from #:lparallel.promise
                #:future)
  (:local-nicknames (#:a #:alexandria)))
(in-package :hunchentoot-extensions/test-async)


(util/fiveam:def-suite)

(defclass my-acceptor (test-acceptor)
  ()
  (:default-initargs
   :name 'my-acceptor))

(defvar *host*)
(defvar *tmp*)

(defmacro defhandler ((uri) () &body body)
  `(better-easy-handler (nil :uri ,uri :acceptor-names '(my-acceptor) :html nil) ()
     ,@body))

(def-fixture state ()
  (with-local-acceptor (*host*) ('my-acceptor)
    (uiop:with-temporary-file (:stream s :pathname p :external-format :latin-1)
      (write-string "bar1" s)
      (finish-output s)
      (with-global-binding ((*tmp* p))
       (&body)))))

(defhandler ("/happy-path") ()
  "foo")

(defun get (url)
  (http-request (format nil "~a~a" *host* url)
                :want-string t))

(test happy-path
  (with-fixture state ()
    (is (equal "foo" (get "/happy-path")))))

(defhandler ("/happy-async-path") ()
  (setf (hunchentoot:header-out :content-type) "image/png")
  (hex:prepare-async-response (r)
    (hex:handle-async-static-file r *tmp*))
  "Bad")

(test happy-async-path
  (with-fixture state ()
    (is (equal "bar1" (get "/happy-async-path")))))

(defhandler ("/happy-really-async-path") ()
  (setf (hunchentoot:header-out :content-type) "image/png")
  (hex:prepare-async-response (r)
    (bt:make-thread
     (lambda ()
       (sleep 0.1)
       (hex:handle-async-static-file r *tmp*))))
  "Bad")

(test happy-really-async-path
  (with-fixture state ()
    (is (equal "bar1" (get "/happy-really-async-path")))))
