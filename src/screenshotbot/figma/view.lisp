;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/figma/view
  (:use #:cl)
  (:import-from #:core/ui/simple-card-page
                #:simple-card-page)
  (:import-from #:screenshotbot/dashboard/compare
                #:associate-figma)
  (:import-from #:hunchentoot-extensions
                #:make-url)
  (:import-from #:nibble
                #:nibble)
  (:import-from #:screenshotbot/figma/figma
                #:figma-client-secret
                #:figma-client-id)
  (:import-from #:screenshotbot/installation
                #:installation-figma))
(in-package :screenshotbot/figma/view)

(named-readtables:in-readtable markup:syntax)

;; TODO: take in image etc.
(defun associate-figma ()

  (let ((submit (nibble (url)
                  (%start-oauth-flow url))))
    <simple-card-page form-action= submit >
      <div class="form-group mb-3">
        <label for="figma-url" class="form-label">Figma Component URL</label>
        <input type="url" 
               class="form-control" 
               id="figma-url" 
               name="figma-url" 
               placeholder="https://www.figma.com/file/..." 
               required />
        <div class="form-text">
          Enter the URL of the Figma component or frame you want to associate
        </div>
      </div>
      <button type="submit" class="btn btn-primary">Associate with Figma</button>
    </simple-card-page>))



(defun %start-oauth-flow (figma-url)
  "Initiates the Figma OAuth flow to get access to the user's Figma files"
  (declare (ignore figma-url))
  (let* ((figma (installation-figma (screenshotbot/installation:installation)))
         (client-id
           (figma-client-id figma))
         (client-secret
           (figma-client-secret figma))
         (redirect-uri (hex:make-full-url hunchentoot:*request* "/account/oauth-callback"))
         (redirect (nibble (code error)
                     (cond
                       (error
                        (error "OAuth error: ~a" error))
                       (code
                        (handle-after-oauth-response
                         figma-url
                         :code code
                         :client-id client-id
                         :client-secret client-secret))
                       (t
                        (error "Invalid OAuth callback state")))))
         (state (nibble:nibble-id redirect))
         (oauth-url (format nil "https://www.figma.com/oauth?client_id=~a&redirect_uri=~a&scope=file_read&state=~a&response_type=code"
                           (quri:url-encode client-id)
                           (quri:url-encode redirect-uri)
                           state)))
    (hex:safe-redirect oauth-url)))

(defun handle-after-oauth-response (figma-url &key code client-secret client-id)
  "Handle the OAuth response by exchanging the code for an access token"
  (let* ((redirect-uri (hex:make-full-url hunchentoot:*request* "/account/oauth-callback"))
         (token-response (util/request:http-request 
                          "https://api.figma.com/v1/oauth/token"
                          :method :post
                          :parameters `(("client_id" . ,client-id)
                                        ("client_secret" . ,client-secret)
                                        ("redirect_uri" . ,redirect-uri)
                                        ("code" . ,code)
                                        ("grant_type" . "authorization_code"))
                          :want-string t))
         (token-data (json:decode-json-from-string token-response))
         (access-token (cdr (assoc :access--token token-data))))
    (declare (ignore figma-url))
    ;; TODO: Store the access token and process the Figma URL
    (error "don't know what to do yet: ~a" access-token)))





