;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/auth-server/resource-server
  (:use #:cl)
  (:import-from #:easy-macros
                #:def-easy-macro)
  (:import-from #:screenshotbot/api/core
                #:api-error
                #:api-error-msg
                #:authenticate-api-request
                #:bearer-token)
  (:import-from #:screenshotbot/auth-server/model
                #:access-token-resource
                #:access-token-scopes
                #:oauth-access-token)
  (:export
   #:bearer-challenge
   #:send-forbidden
   #:send-unauthorized
   #:token-has-scope-p
   #:token-issued-for-p
   #:with-bearer-authentication)
  (:documentation "Helpers for endpoints acting as OAuth 2.0 resource servers.

The important part is the 401: RFC 6750 §3 defines the challenge, RFC
9728 §5.1 adds the `resource_metadata` parameter, and the MCP
authorization spec makes that parameter mandatory. It is the only thing
standing between a client that has seen nothing but this URL and knowing
where to go and authenticate, so a 401 without it is a dead end."))
(in-package :screenshotbot/auth-server/resource-server)

(defun %sanitize (value)
  "Strip anything that could break out of a quoted header parameter.

Everything we put in a challenge is our own text today, but a header
built by FORMAT from values that might one day not be is worth closing
off now."
  (when value
    (remove-if (lambda (ch)
                 (member ch '(#\" #\\ #\Return #\Newline)))
               (princ-to-string value))))

(defun bearer-challenge (resource-metadata-url &key error description scope)
  "The WWW-Authenticate value for a rejected request.

RFC 6750 §3.1 says not to send an error code when the request carried no
credentials at all: nothing was wrong with the token, there just wasn't
one. So ERROR and DESCRIPTION are omitted in that case, and the challenge
is a plain invitation to authenticate.

SCOPE names what the endpoint needed. §3 allows it, and it is the
difference between a client that can fix itself on the next authorization
request and one that can only guess."
  (format nil "Bearer resource_metadata=\"~a\"~@[, error=\"~a\"~]~@[, error_description=\"~a\"~]~@[, scope=\"~a\"~]"
          (%sanitize resource-metadata-url)
          (%sanitize error)
          (%sanitize description)
          (%sanitize scope)))

(defun %send-challenge (status resource-metadata-url &key error description scope)
  (setf (hunchentoot:return-code*) status)
  (setf (hunchentoot:header-out :www-authenticate)
        (bearer-challenge resource-metadata-url
                          :error error
                          :description description
                          :scope scope))
  (setf (hunchentoot:content-type*) "application/json; charset=utf-8")
  (setf (hunchentoot:header-out :cache-control) "no-store")
  ;; The id is null because we reject before parsing the request, so we
  ;; never learn which call this was. JSON-RPC 2.0 §5 allows that.
  (json:encode-json-to-string
   `((:jsonrpc . "2.0")
     (:id . nil)
     (:error . ((:code . -32001)
                (:message . ,(or description "Unauthorized")))))))

(defun send-unauthorized (resource-metadata-url &key error description)
  "Answer 401: we do not know who this is."
  (%send-challenge hunchentoot:+http-authorization-required+
                   resource-metadata-url
                   :error error
                   :description description))

(defun send-forbidden (resource-metadata-url &key scope)
  "Answer 403: we know who this is, and they are not allowed to be here.

RFC 6750 §3.1 pairs `insufficient_scope` with 403 rather than 401
precisely so a client does not re-authenticate in a loop -- a fresh token
with the same scopes would fail identically."
  (%send-challenge hunchentoot:+http-forbidden+
                   resource-metadata-url
                   :error "insufficient_scope"
                   :description (format nil "This endpoint requires the ~a scope"
                                        scope)
                   :scope scope))

(defun %failure-description (condition)
  "What to tell the client about a failed authentication.

API-ERROR messages are written for API callers and are safe to pass on;
anything else could be an internal detail, so it gets a flat answer."
  (typecase condition
    (api-error (api-error-msg condition))
    (t "The access token is not valid")))

(defun token-issued-for-p (api-key resource)
  "Was API-KEY issued for RESOURCE, per RFC 8707?

Anything that is not an audience-bound OAuth token fails: a dashboard API
key, a CLI key, or an OAuth token minted before its client started
sending `resource`. That is the point rather than an oversight -- the MCP
authorization spec requires the resource server to confirm the token was
issued for *it*, and 'no audience recorded' is not a confirmation.

Failing closed also means a leaked API key is not automatically an MCP
key, which is the compartmentalisation audience binding exists to buy."
  (and (typep api-key 'oauth-access-token)
       (equal resource (access-token-resource api-key))
       t))

(defun token-has-scope-p (api-key scope)
  "Does API-KEY carry SCOPE?

Only OAuth tokens have scopes at all, so an API key fails here for the
same reason it fails TOKEN-ISSUED-FOR-P: nothing about it says a user
agreed to this particular access."
  (and (typep api-key 'oauth-access-token)
       (member scope (access-token-scopes api-key) :test #'equal)
       t))

(def-easy-macro with-bearer-authentication (&key resource-metadata-url resource
                                            scope &fn fn)
  "Run FN only if the request carries a bearer token issued for RESOURCE
and carrying SCOPE.

On success the request has a user, an account and a viewer context bound,
exactly as it would for any other API call -- this reuses
AUTHENTICATE-API-REQUEST rather than growing a second auth path.

RESOURCE is the RFC 8707 identifier this endpoint answers to, and SCOPE
what the user must have agreed to. Either may be NIL to skip that check,
which is what an endpoint with no resource identifier or no scope
requirement wants."
  (block authenticated
    (let ((presented (bearer-token)))
      (flet ((reject (description)
               (return-from authenticated
                 (send-unauthorized
                  resource-metadata-url
                  ;; RFC 6750 §3.1: only describe an error if they
                  ;; actually gave us something to reject.
                  :error (when presented "invalid_token")
                  :description (when presented description)))))
        (let ((api-key
                (handler-bind
                    ;; An unauthenticated probe is not worth a log line;
                    ;; this endpoint is public and gets scanned. A *bad*
                    ;; token still warns, because that is worth seeing.
                    ((warning (lambda (w)
                                (unless presented
                                  (muffle-warning w)))))
                  (handler-case
                      (authenticate-api-request hunchentoot:*request*)
                    (error (e)
                      (reject (%failure-description e)))))))
          (when (and resource
                     (not (token-issued-for-p api-key resource)))
            ;; The token is real, and belongs to someone -- it was just
            ;; never meant for this API. Saying so plainly beats a generic
            ;; rejection, because the fix is for the client to ask for the
            ;; right resource, and it cannot guess that from silence.
            (reject (format nil "This token was not issued for ~a" resource)))
          (when (and scope
                     (not (token-has-scope-p api-key scope)))
            ;; 403, not 401: the caller is who they say they are, and
            ;; re-authenticating with the same scopes would fail the same
            ;; way. The fix is a new authorization request asking for
            ;; SCOPE, which the challenge names.
            (return-from authenticated
              (send-forbidden resource-metadata-url :scope scope)))
          ;; AUTHENTICATE-API-REQUEST has bound the user and account on
          ;; the request, so the body reads them through AUTH: as usual.
          (funcall fn))))))
