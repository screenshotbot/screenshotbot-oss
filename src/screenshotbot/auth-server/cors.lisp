;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/auth-server/cors
  (:use #:cl)
  (:export
   #:allow-cross-origin
   #:preflight
   #:+allowed-request-headers+
   #:+preflight-max-age+)
  (:documentation "Cross-origin access for the OAuth and MCP endpoints.

A browser-based MCP client runs on somebody else's origin, so without
these headers it cannot read the token endpoint's reply, and -- worse --
cannot read the WWW-Authenticate challenge on a 401, which is the whole
mechanism by which it discovers where to authenticate. That failure is
silent: the browser strips the header and the client sees an opaque
rejection with nothing to act on."))
(in-package :screenshotbot/auth-server/cors)

(defparameter +allowed-request-headers+
  "Authorization, Content-Type, MCP-Protocol-Version"
  "Request headers a browser client may send.

Content-Type because JSON-RPC bodies are application/json, which is not a
CORS-safelisted value and so triggers preflight on its own; Authorization
for the bearer token; MCP-Protocol-Version because the MCP spec has
clients send it on every call.")

(defparameter +preflight-max-age+ "86400"
  "A day. These values never change at runtime, and re-preflighting every
call would double the request count for no benefit.")

(defun allow-cross-origin (&key expose)
  "Let a browser client on any origin read this response.

`*` rather than an allow-list because none of the endpoints that call
this take ambient authority: credentials arrive in the request body or an
Authorization header, never a cookie. Browsers refuse to send cookies to
a `*` origin, so a hostile page gains nothing -- it can only make the
same unauthenticated request it could already have made from a server it
controls.

That reasoning is also why /oauth/authorize deliberately does NOT call
this. It is a cookie-authenticated browser navigation, and making it
cross-origin readable would hand any page the user's live authorization
screen.

No `Vary: Origin`, because the value does not vary by origin."
  (setf (hunchentoot:header-out :access-control-allow-origin) "*")
  (when expose
    ;; Without this the browser hands the client a response whose headers
    ;; have been stripped down to the CORS-safelisted ones.
    (setf (hunchentoot:header-out :access-control-expose-headers) expose))
  (values))

(defun preflight (&key (methods "POST, OPTIONS"))
  "Answer a CORS preflight for an endpoint accepting METHODS."
  (allow-cross-origin)
  (setf (hunchentoot:header-out :access-control-allow-methods) methods)
  (setf (hunchentoot:header-out :access-control-allow-headers)
        +allowed-request-headers+)
  (setf (hunchentoot:header-out :access-control-max-age) +preflight-max-age+)
  (setf (hunchentoot:return-code*) hunchentoot:+http-no-content+)
  "")
