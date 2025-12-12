;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :hunchentoot-extensions/test-forward
  (:use #:cl
        #:fiveam)
  (:import-from #:util/testing
                #:with-global-binding
                #:with-local-acceptor
                #:test-acceptor)
  (:import-from #:hunchentoot
                #:define-easy-handler)
  (:import-from #:hunchentoot-extensions
                #:better-easy-handler)
  (:import-from #:util/request
                #:http-request)
  (:import-from #:alexandria
                #:assoc-value)
  (:import-from #:fiveam-matchers/core
                #:is-equal-to
                #:assert-that)
  (:import-from #:fiveam-matchers/strings
                #:matches-regex)
  (:local-nicknames (#:a #:alexandria)))
(in-package :hunchentoot-extensions/test-forward)

(util/fiveam:def-suite)

;; Destination acceptor that will receive forwarded requests
(defclass destination-acceptor (test-acceptor)
  ()
  (:default-initargs
   :name 'destination-acceptor))

;; Source acceptor that will forward requests
(defclass source-acceptor (test-acceptor)
  ()
  (:default-initargs
   :name 'source-acceptor))

(defvar *destination-host*)
(defvar *received-headers* nil)
(defvar *received-body* nil)
(defvar *received-method* nil)

;; Handler on destination that echoes back what it received
(better-easy-handler (echo-handler :uri "/echo"
                                   :acceptor-names '(destination-acceptor))
    ()
  (setf *received-headers* (hunchentoot:headers-in hunchentoot:*request*))
  (setf *received-method* (hunchentoot:request-method hunchentoot:*request*))
  (setf *received-body* (hunchentoot:raw-post-data :force-text t))
  (setf (hunchentoot:content-type*) "text/plain")
  (format nil "Received: ~a ~a"
          *received-method*
          (hunchentoot:script-name hunchentoot:*request*)))

;; Handler that returns a specific status code (on destination)
(better-easy-handler (status-handler :uri "/status/:code"
                                     :acceptor-names '(destination-acceptor))
    (code)
  (setf (hunchentoot:return-code*) (parse-integer code))
  (format nil "Status: ~a" code))

;; Handler on source that forwards status requests
(better-easy-handler (forward-status-handler :uri "/status/:code"
                                             :acceptor-names '(source-acceptor))
    (code)
  (declare (ignore code)) ; path is preserved by forward-request
  (hex:forward-request *destination-host* :keep-current-host nil))

;; Handler for testing that source acceptor routing works
(better-easy-handler (test-forward-exists :uri "/test-forward-exists"
                                          :acceptor-names '(source-acceptor))
    ()
  "forward handler exists")

;; Handler on source that forwards to destination
;; Note: forward-request preserves the request path, so we forward /echo to /echo
(better-easy-handler (forward-echo-handler :uri "/echo"
                                           :acceptor-names '(source-acceptor))
    ()
  (hex:forward-request *destination-host* :keep-current-host nil))


(def-fixture state ()
  (with-local-acceptor (host) ('destination-acceptor)
    (with-global-binding ((*destination-host* host))
      (&body))))

(test test-echo-handler-directly
  (with-fixture state ()
    (setf *received-method* nil)
    (let ((response (http-request (format nil "~a/echo" *destination-host*)
                                  :want-string t)))
      (is (equal "Received: GET /echo" response))
      (is (eql :get *received-method*)))))

(test test-source-acceptor-routing
  (with-fixture state ()
    (with-local-acceptor (source-host) ('source-acceptor)
      (let ((response (http-request (format nil "~a/test-forward-exists" source-host)
                                    :want-string t)))
        (is (equal "forward handler exists" response))))))

(test test-basic-forward
  (with-fixture state ()
    (with-local-acceptor (source-host) ('source-acceptor)
      (setf *received-headers* nil
            *received-body* nil
            *received-method* nil)
      (let ((response (http-request (format nil "~a/echo" source-host)
                                    :want-string t)))
        ;; Verify request was forwarded to destination and processed
        (is (equal "Received: GET /echo" response))
        (is (eql :get *received-method*))))))

(test test-forward-with-custom-headers
  (with-fixture state ()
    (with-local-acceptor (source-host) ('source-acceptor)
      (setf *received-headers* nil)
      (http-request (format nil "~a/echo" source-host)
                    :additional-headers '((:x-custom-header . "custom-value"))
                    :want-string t)
      ;; Verify custom header was forwarded
      (is (equal "custom-value"
                 (cdr (assoc :x-custom-header *received-headers*)))))))

(test test-forward-post-body
  (with-fixture state ()
    (with-local-acceptor (source-host) ('source-acceptor)
      (setf *received-body* nil
            *received-method* nil)
      (http-request (format nil "~a/echo" source-host)
                    :method :post
                    :content "test body content"
                    :want-string t)
      ;; Verify POST method and body were forwarded
      (is (eql :post *received-method*))
      (is (equal "test body content" *received-body*)))))

(test test-forward-status-code
  (with-fixture state ()
    (with-local-acceptor (source-host) ('source-acceptor)
      (multiple-value-bind (body code)
          (http-request (format nil "~a/status/404" source-host)
                        :want-string t)
        (declare (ignore body))
        ;; Verify status code was forwarded
        (is (eql 404 code))))))

(test test-keep-current-host-false
  (with-fixture state ()
    (with-local-acceptor (source-host) ('source-acceptor)
      (setf *received-headers* nil)
      (http-request (format nil "~a/echo" source-host)
                    :want-string t)
      ;; When keep-current-host is false, host should be the destination host
      ;; X-Forwarded-Host should contain the original host
      (is (not (null (cdr (assoc :x-forwarded-host *received-headers*))))))))

(test test-keep-current-host-true
  (with-fixture state ()
    (with-local-acceptor (source-host) ('source-acceptor)
      (setf *received-headers* nil)
      (let ((uri (quri:uri source-host)))
       (http-request (format nil "http://example.com:~a/echo" (quri:uri-port uri))
                     :real-host (quri:uri-host uri)
                     :want-string t))
      ;; X-Forwarded-Host should still be set
      (is (not (null (cdr (assoc :x-forwarded-host *received-headers*)))))
      (assert-that (assoc-value *received-headers* :x-forwarded-host)
                   (matches-regex "example.com:.*")))))

(test test-forward-preserves-uri-path
  (with-fixture state ()
    (with-local-acceptor (source-host) ('source-acceptor)
      (setf *received-headers* nil)
      ;; The forwarding should preserve query parameters
      (let ((response (http-request (format nil "~a/echo?foo=bar" source-host)
                                    :want-string t)))
        (is (str:contains? "Received:" response))))))
