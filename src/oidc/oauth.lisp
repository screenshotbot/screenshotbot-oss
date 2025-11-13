;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :oidc/oauth
    (:use #:cl)
  (:import-from #:nibble
                #:nibble
                #:nibble-id)
  (:import-from #:alexandria
                #:remove-from-plist)
  (:export #:make-oauth-url))
(in-package :oidc/oauth)

(defun make-oauth-url (url callback &rest args &key redirect-uri &allow-other-keys)
  "Takes a base OAuth URL and a redirect nibble, and returns the complete URL
   with redirect_uri and state parameters added.

CALLBACK is called with three named arguments: code, error and the
redirect-uri used. You can ignore the redirect-uri, but in many cases
you need the redirect-uri to request the token later.

REDIRECT-URI overrides the redirect-uri. This is only useful for our
GitHub implementation which is trying to redirect to
accounts.screenshotbot.io

Any other arguments will be added like (hex:make-url ... ),
i.e. encoded as http arguments"

  (let ((url (apply #'hex:make-url url (remove-from-plist args :redirect-uri))))
   (let* ((redirect-uri (or
                         redirect-uri
                         (hex:make-full-url hunchentoot:*request* "/account/oauth-callback")))
          (redirect-nibble (nibble (code error error_description)
                             (funcall callback
                                      :redirect-uri redirect-uri
                                      :code code
                                      :error-description error_description
                                      :error error
                                      :allow-other-keys t)))
          (state (nibble-id redirect-nibble))
          (uri (quri:uri url)))
     (quri:render-uri
      (quri:make-uri
       :defaults uri
       :query (append (quri:uri-query-params uri)
                      `(("redirect_uri" . ,redirect-uri)
                        ("state" . ,(princ-to-string state)))))))))
