;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :auth-server/client/loopback
  (:use #:cl)
  (:import-from #:auth-server/client/conditions
                #:authorization-timeout)
  (:import-from #:easy-macros
                #:def-easy-macro)
  (:import-from #:hunchentoot-extensions/random-port
                #:acceptor-on-random-port)
  (:export
   #:loopback-listener
   #:listener-port
   #:listener-path
   #:listener-redirect-uri
   #:start-listener
   #:stop-listener
   #:await-callback
   #:with-loopback-listener
   #:+callback-path+)
  (:documentation "The loopback redirect receiver, RFC 8252 §7.3.

A CLI can't register a fixed redirect URI, so it binds an ephemeral port
on 127.0.0.1 and has the browser deliver the authorization code straight
back to it. The authorization server is required to allow any port for
loopback redirects precisely so this works.

The serving is hunchentoot's: ACCEPTOR-ON-RANDOM-PORT already binds port
0 and reports back the port it actually got, including the LispWorks
branch where hunchentoot uses comm rather than usocket. Hand-rolling an
accept loop here bought nothing and cost a Darwin-only bug."))
(in-package :auth-server/client/loopback)

(defparameter +callback-path+ "/callback"
  "Must match a path registered for the client. The port is flexible, the
path is not.")

(defclass loopback-listener (acceptor-on-random-port
                             hunchentoot:acceptor)
  ((path :initarg :path
         :initform +callback-path+
         :reader listener-path)
   (received :initform nil
             :accessor %received
             :documentation "Separate from RESULT: a redirect can legitimately
carry no query at all, so RESULT being NIL is not the same as nothing
having arrived.")
   (result :initform nil
           :accessor %result)
   (lock :initform (bt:make-lock "oauth-loopback")
         :reader %lock))
  (:default-initargs
   ;; ACCEPTOR-ON-RANDOM-PORT binds 127.0.0.1 itself; this matches so
   ;; nothing downstream thinks we're listening on every interface.
   :address "127.0.0.1"
   ;; A CLI's stdout belongs to the CLI.
   :access-log-destination nil
   :message-log-destination nil))

(defmethod listener-port ((self loopback-listener))
  (hunchentoot:acceptor-port self))

(defmethod listener-redirect-uri ((self loopback-listener))
  (format nil "http://127.0.0.1:~a~a" (listener-port self) (listener-path self)))

(defun start-listener (&key (path +callback-path+))
  (let ((listener (make-instance 'loopback-listener :path path)))
    (hunchentoot:start listener)
    listener))

(defun stop-listener (listener)
  (ignore-errors
   (hunchentoot:stop listener)))

(def-easy-macro with-loopback-listener (&binding listener &key path &fn fn)
  (let ((listener (if path (start-listener :path path) (start-listener))))
    (unwind-protect
         (funcall fn listener)
      (stop-listener listener))))

;; ----------------------------------------------------------------------
;; Serving
;; ----------------------------------------------------------------------

(defun %page (title detail)
  (format nil "<html><head><title>~a</title></head>~
<body style=\"font-family:sans-serif;padding:3em\">~
<h2>~a</h2><p>~a</p></body></html>"
          title title detail))

(defun %result-page (params)
  (let ((error (cdr (assoc "error" params :test #'equal))))
    (if error
        (%page "Authorization failed"
               (or (cdr (assoc "error_description" params :test #'equal)) error))
        (%page "You're signed in" "You can close this tab and go back to the terminal."))))

(defmethod hunchentoot:acceptor-dispatch-request ((acceptor loopback-listener) request)
  (setf (hunchentoot:content-type*) "text/html; charset=utf-8")
  (cond
    ((equal (hunchentoot:script-name request) (listener-path acceptor))
     (let ((params (hunchentoot:get-parameters request)))
       (bt:with-lock-held ((%lock acceptor))
         ;; Keep the first one. If the user reloads the tab we must not
         ;; overwrite a good result with a replayed or empty one.
         (unless (%received acceptor)
           (setf (%result acceptor) params)
           (setf (%received acceptor) t)))
       (%result-page params)))
    (t
     ;; Browsers speculatively fetch /favicon.ico and friends; one of
     ;; those must not be mistaken for the redirect.
     (setf (hunchentoot:return-code*) hunchentoot:+http-not-found+)
     (%page "Not found" "Nothing here."))))

(defun await-callback (listener &key (timeout 300))
  "Block until the browser delivers the redirect, returning its query as an alist."
  (let* ((start (get-universal-time))
         (deadline (+ start timeout)))
    (loop
      (bt:with-lock-held ((%lock listener))
        (when (%received listener)
          (return (%result listener))))
      (when (>= (get-universal-time) deadline)
        ;; Report elapsed rather than the configured timeout: when the two
        ;; disagree, that difference is the bug.
        (error 'authorization-timeout :seconds (- (get-universal-time) start)))
      ;; A human is on the other end of this; 100ms of latency is free and
      ;; costs far less complexity than a condition variable.
      (sleep 0.1))))
