;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :auth-server/client/token-store
  (:use #:cl)
  (:import-from #:auth-server/client/http
                #:decode-json
                #:field)
  (:export
   #:token-set
   #:access-token-string
   #:refresh-token-string
   #:token-expires-at
   #:token-scope
   #:token-host
   #:token-usable-p
   #:tokens-from-response
   #:default-token-file
   #:load-tokens
   #:save-tokens
   #:forget-tokens))
(in-package :auth-server/client/token-store)

(defparameter +clock-slack+ 60
  "Treat a token as spent this many seconds early, so we never hand over
one that expires while the request it's for is still in flight.")

(defclass token-set ()
  ((access-token :initarg :access-token
                 :reader access-token-string)
   (refresh-token :initarg :refresh-token
                  :initform nil
                  :reader refresh-token-string)
   (expires-at :initarg :expires-at
               :reader token-expires-at
               :documentation "Universal time. Absolute, because a relative
expires_in is meaningless once it's been sitting in a file.")
   (scope :initarg :scope
          :initform nil
          :reader token-scope)
   (host :initarg :host
         :reader token-host
         :documentation "The installation these tokens came from. Kept so we
never present a staging token to production."))
  (:documentation "What a token endpoint gave us, plus what we need to
decide whether it's still good."))

(defun tokens-from-response (response host)
  (make-instance 'token-set
                 :access-token (field response "access_token")
                 :refresh-token (field response "refresh_token")
                 :scope (field response "scope")
                 :host host
                 :expires-at (+ (get-universal-time)
                                (or (field response "expires_in") 3600))))

(defmethod token-usable-p ((self token-set))
  (> (token-expires-at self) (+ (get-universal-time) +clock-slack+)))

(defun default-token-file ()
  (merge-pathnames ".config/screenshotbot/oauth-token.json"
                   (user-homedir-pathname)))

(defun load-tokens (path host)
  "The cached tokens for HOST, or NIL.

A cache miss is never worth an error: the caller's fallback is to sign in
again, which is exactly what we want when the file is absent, corrupt, or
from a different installation."
  (let ((payload (ignore-errors
                  (decode-json (uiop:read-file-string path)))))
    (when (and payload (equal host (field payload "host")))
      (make-instance 'token-set
                     :access-token (field payload "access_token")
                     :refresh-token (field payload "refresh_token")
                     :scope (field payload "scope")
                     :host (field payload "host")
                     :expires-at (or (field payload "expires_at") 0)))))

(defun %restrict-permissions (path)
  "Make PATH readable only by its owner.

The file holds a bearer credential. There's no portable CL chmod, so on
unix we shell out; on Windows the user profile directory is already
per-user."
  (unless (uiop:os-windows-p)
    (ignore-errors
     (uiop:run-program (list "chmod" "600" (namestring path))
                       :ignore-error-status t))))

(defun save-tokens (path host response)
  "Persist RESPONSE, returning the TOKEN-SET it represents."
  (let ((tokens (tokens-from-response response host)))
    (ensure-directories-exist path)
    ;; Create the file empty and lock it down *before* the secret goes in,
    ;; so it is never briefly world-readable.
    (with-open-file (stream path :direction :output :if-exists :supersede)
      (declare (ignore stream)))
    (%restrict-permissions path)
    (with-open-file (stream path :direction :output :if-exists :supersede)
      (write-string
       (json:encode-json-alist-to-string
        `(("access_token" . ,(access-token-string tokens))
          ("refresh_token" . ,(refresh-token-string tokens))
          ("scope" . ,(token-scope tokens))
          ("host" . ,(token-host tokens))
          ("expires_at" . ,(token-expires-at tokens))))
       stream))
    tokens))

(defun forget-tokens (path)
  (ignore-errors (delete-file path)))
