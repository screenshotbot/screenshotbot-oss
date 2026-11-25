;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/curl/curl-request
  (:use #:cl)
  (:import-from #:util/request
                #:engine
                #:low-level-request
                #:network-error
                #:connection-error
                #:timeout-error)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:curl-engine
   #:*curl-engine*
   #:curl-error
   #:curl-error-code
   #:libcurl-available-p
   #:*libcurl-enabled-p*))
(in-package :util/curl/curl-request)

(defclass curl-engine (engine)
  ()
  (:documentation "An engine that makes requests with libcurl, which takes care of
keeping connections alive and reusing them, HTTP/2, decompression and
TLS.

This is only implemented on LispWorks on Linux and Mac (see
curl-request.lisp). Everywhere else, and for the few arguments we don't
handle (e.g. :COOKIE-JAR or :STREAM), and when libcurl can't be loaded,
it behaves exactly like ENGINE, i.e. it uses Drakma."))

(defvar *curl-engine* (make-instance 'curl-engine))

(defvar *libcurl-enabled-p* t
  "Set to NIL to make CURL-ENGINE fall back to Drakma for every request.")

(define-condition curl-error (network-error)
  ((code :initarg :code
         :initform nil
         :reader curl-error-code)
   (message :initarg :message
            :initform nil
            :reader curl-error-message)
   (url :initarg :url
        :initform nil
        :reader curl-error-url))
  (:report (lambda (e out)
             (format out "libcurl error ~a~@[ for ~a~]: ~a"
                     (curl-error-code e)
                     (curl-error-url e)
                     (curl-error-message e)))))

(define-condition curl-connection-error (curl-error connection-error)
  ())

(define-condition curl-timeout-error (curl-error timeout-error)
  ())

(define-condition curl-connect-timeout-error (curl-timeout-error connection-error)
  ()
  (:documentation "We timed out before we had a connection. This is a CONNECTION-ERROR
too, so that WRAP-SSL-ERRORS turns it into a 502 like Drakma's
SOCKET-CREATE-ERROR."))

#-(and lispworks (not (or mswindows win32)))
(defun libcurl-available-p ()
  nil)
