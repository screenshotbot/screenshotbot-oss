;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :auth-server/client/discovery
  (:use #:cl)
  (:import-from #:auth-server/client/http
                #:field
                #:get-json)
  (:export
   #:server-metadata
   #:authorization-endpoint
   #:token-endpoint
   #:device-authorization-endpoint
   #:revocation-endpoint
   #:metadata-host
   #:conventional-metadata
   #:discover
   #:repoint)
  (:documentation "OAuth 2.0 Authorization Server Metadata, RFC 8414."))
(in-package :auth-server/client/discovery)

(defclass server-metadata ()
  ((host :initarg :host
         :reader metadata-host)
   (authorization-endpoint :initarg :authorization-endpoint
                           :reader authorization-endpoint)
   (token-endpoint :initarg :token-endpoint
                   :reader token-endpoint)
   (device-authorization-endpoint :initarg :device-authorization-endpoint
                                  :reader device-authorization-endpoint)
   (revocation-endpoint :initarg :revocation-endpoint
                        :reader revocation-endpoint)))

(defun %url (host path)
  (quri:render-uri (quri:merge-uris path (string-right-trim "/" host))))

(defun conventional-metadata (host)
  "The endpoints a Screenshotbot installation has, assumed rather than asked."
  (make-instance 'server-metadata
                 :host host
                 :authorization-endpoint (%url host "/oauth/authorize")
                 :token-endpoint (%url host "/oauth/token")
                 :device-authorization-endpoint (%url host "/oauth/device/code")
                 :revocation-endpoint (%url host "/oauth/revoke")))

(defun repoint (url host)
  "Keep URL's path, but move it onto HOST.

An installation advertises its canonical issuer, which for a dev or
staging server is often not the address we actually reached it on.
Following the advertised host verbatim would send us somewhere we can't
reach, or -- worse, if someone got the config wrong -- somewhere else
entirely. The paths are the useful part of discovery; the host we already
know."
  (let ((target (quri:uri host))
        (uri (quri:uri url)))
    (quri:render-uri
     (quri:make-uri :scheme (quri:uri-scheme target)
                    :host (quri:uri-host target)
                    :port (quri:uri-port target)
                    :path (quri:uri-path uri)
                    :query (quri:uri-query uri)))))

(defun discover (host &key (use-discovery t))
  "Fetch RFC 8414 metadata for HOST, falling back to the conventional paths.

Discovery is a convenience, not a requirement, so a server that doesn't
publish metadata still works."
  (let ((fallback (conventional-metadata host)))
    (cond
      ((not use-discovery)
       fallback)
      (t
       (let ((document
               (ignore-errors
                (get-json (%url host "/.well-known/oauth-authorization-server")))))
         (cond
           ((not (and document (field document "token_endpoint")))
            (log:debug "No usable metadata at ~a, assuming the standard paths" host)
            fallback)
           (t
            (flet ((endpoint (name default)
                     (let ((advertised (field document name)))
                       (if advertised (repoint advertised host) default))))
              (make-instance
               'server-metadata
               :host host
               :authorization-endpoint
               (endpoint "authorization_endpoint" (authorization-endpoint fallback))
               :token-endpoint
               (endpoint "token_endpoint" (token-endpoint fallback))
               :device-authorization-endpoint
               (endpoint "device_authorization_endpoint"
                         (device-authorization-endpoint fallback))
               :revocation-endpoint
               (endpoint "revocation_endpoint" (revocation-endpoint fallback)))))))))))
