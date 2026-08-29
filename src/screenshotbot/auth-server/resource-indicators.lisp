;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/auth-server/resource-indicators
  (:use #:cl)
  (:import-from #:screenshotbot/auth-server/errors
                #:oauth-error!)
  (:import-from #:screenshotbot/auth-server/protected-resource
                #:known-resource-p)
  (:export
   #:resource-parameters
   #:read-resource
   #:validate-resource!
   #:resource-matches-p
   #:narrowed-resource)
  (:documentation "Resource Indicators for OAuth 2.0, RFC 8707.

A `resource` parameter says which API the client intends to use the token
at, and the authorization server binds the issued token's audience to it.
Without that binding every token is a bearer of everything: a token minted
for the CLI would be equally valid at the MCP endpoint, and a token handed
to one MCP server could be replayed against another. The MCP authorization
spec makes this mandatory for exactly that reason.

This file only decides what a valid indicator is; storing it is the
model's job and enforcing it is the resource server's."))
(in-package :screenshotbot/auth-server/resource-indicators)

(defun resource-parameters (parameters)
  "Every `resource` value in PARAMETERS, an alist of (name . value)."
  (loop for (name . value) in parameters
        if (and (equal "resource" name)
                (not (str:emptyp value)))
          collect value))

(defun validate-resource! (resource)
  "Signal an OAUTH-ERROR unless RESOURCE is one we would issue a token for.

`invalid_target` is RFC 8707 §2.3's error code for this."
  (let ((uri (ignore-errors (quri:uri resource))))
    (unless (and uri (quri:uri-scheme uri) (quri:uri-host uri))
      (oauth-error! "invalid_target"
                    "resource must be an absolute URI"))
    (when (quri:uri-fragment uri)
      ;; RFC 8707 §2. A fragment never reaches a server, so a token
      ;; audienced to one would have an audience nobody could check.
      (oauth-error! "invalid_target"
                    "resource must not contain a fragment"))
    (unless (known-resource-p resource)
      (oauth-error! "invalid_target"
                    (format nil "Unknown resource: ~a" resource))))
  resource)

(defun read-resource (parameters)
  "The resource indicator in PARAMETERS, or NIL if the client didn't ask.

NIL is allowed: an audience-less token still works everywhere that
doesn't demand one, which is what keeps existing CLI tokens valid."
  (let ((values (resource-parameters parameters)))
    (cond
      ((null values)
       nil)
      ((cdr values)
       ;; RFC 8707 permits several, to mint one token good at several
       ;; resources. We have one resource; accepting a list and then
       ;; honouring only part of it is worse than refusing it outright.
       (oauth-error! "invalid_target"
                     "Only one resource indicator is supported"))
      (t
       (validate-resource! (first values))))))

(defun resource-matches-p (requested issued)
  "Is REQUESTED an acceptable audience for a token whose grant carried ISSUED?

Asking for nothing keeps whatever the earlier step established, which is
what lets a client send `resource` once at the authorization endpoint and
omit it at the token endpoint."
  (or (null requested)
      (equal requested issued)))

(defun narrowed-resource (requested issued)
  "The audience to stamp on a new token.

Signals if the client is trying to widen: a token exchange can only ever
produce something no more powerful than what was authorized."
  (cond
    ((null requested)
     issued)
    ((equal requested issued)
     issued)
    (t
     (oauth-error! "invalid_target"
                   (format nil "This authorization was not issued for ~a"
                           requested)))))
