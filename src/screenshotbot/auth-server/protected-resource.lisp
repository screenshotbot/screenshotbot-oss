;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/auth-server/protected-resource
  (:use #:cl)
  (:import-from #:core/installation/installation
                #:*installation*
                #:installation-domain)
  (:import-from #:screenshotbot/auth-server/cors
                #:allow-cross-origin)
  (:import-from #:screenshotbot/server
                #:defhandler)
  (:export
   #:+mcp-path+
   #:mcp-resource-identifier
   #:mcp-resource-metadata-url
   #:mcp-resource-metadata
   #:known-resource-identifiers
   #:known-resource-p)
  (:documentation "OAuth 2.0 Protected Resource Metadata, RFC 9728.

An MCP client that has only been given a hostname needs to find out which
authorization server guards the MCP endpoint. It learns that here, and it
is pointed here by the `WWW-Authenticate` challenge on an unauthenticated
request -- see the MCP authorization spec, which makes both mandatory.

Only the MCP endpoint is described for now. If the REST API ever needs
its own document, this becomes a registry keyed by resource path rather
than a pair of functions."))
(in-package :screenshotbot/auth-server/protected-resource)

(defparameter +mcp-path+ "/mcp"
  "Where the MCP server is mounted. See SCREENSHOTBOT/MCP/MCP.")

(defun %url (path)
  (quri:render-uri
   (quri:merge-uris path (installation-domain *installation*))))

(defun mcp-resource-identifier ()
  "The canonical URI identifying the MCP server as a resource.

This is the value MCP clients will send as the `resource` parameter once
we implement RFC 8707, and the audience their tokens get bound to, so it
has to stay stable: changing it invalidates every issued token's
audience."
  (%url +mcp-path+))

(defun mcp-resource-metadata-url ()
  "Where MCP-RESOURCE-METADATA is served.

RFC 9728 §3.1 builds this by inserting the resource's path *after* the
well-known segment, so a resource at /mcp is described at
/.well-known/oauth-protected-resource/mcp -- not at the bare well-known
path, which would describe the origin itself."
  (%url (format nil "/.well-known/oauth-protected-resource~a" +mcp-path+)))

(defun known-resource-identifiers ()
  "Every resource this installation will audience-bind a token to.

RFC 8707 §2 says the authorization server SHOULD reject an unknown
target. Without that a client could ask for, and be handed, a token
audienced to a URI we have nothing to do with -- which is exactly the
confused-deputy shape resource indicators exist to prevent."
  (list (mcp-resource-identifier)))

(defun known-resource-p (resource)
  "Exact match, deliberately. Prefix matching would let /mcp-evil pass as
/mcp, and we know our own resources by name."
  (and resource
       (member resource (known-resource-identifiers) :test #'equal)
       t))

(defun mcp-resource-metadata ()
  `(("resource" . ,(mcp-resource-identifier))
    ;; Same installation: we are our own authorization server, which is
    ;; why opaque tokens suffice and there is no jwks_uri here.
    ("authorization_servers" ,(installation-domain *installation*))
    ;; What the MCP endpoint actually requires today. A dedicated mcp:
    ;; scope belongs with the change that gives MCP its own permissions.
    ("scopes_supported" "api:read")
    ;; RFC 6750 §2.1. We deliberately don't accept the form-encoded or
    ;; query-parameter variants: a token in a URL ends up in logs.
    ("bearer_methods_supported" "header")
    ("resource_name" . "Screenshotbot MCP")
    ("resource_documentation" . ,(%url "/documentation/api"))))

(defhandler (nil :uri "/.well-known/oauth-protected-resource/mcp" :method :get) ()
  (allow-cross-origin)
  (setf (hunchentoot:content-type*) "application/json; charset=utf-8")
  ;; RFC 9728 §3.3: public and cacheable, like the authorization server
  ;; document next to it.
  (setf (hunchentoot:header-out :cache-control) "max-age=3600")
  (json:encode-json-alist-to-string (mcp-resource-metadata)))
