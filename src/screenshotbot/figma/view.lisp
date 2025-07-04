;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/figma/view
  (:use #:cl)
  (:import-from #:core/ui/simple-card-page
                #:simple-card-page)
  (:import-from #:hunchentoot-extensions
                #:make-url)
  (:import-from #:nibble
                #:nibble)
  (:import-from #:screenshotbot/figma/figma
                #:figma-client-secret
                #:figma-client-id)
  (:import-from #:screenshotbot/installation
                #:installation-figma)
  (:import-from #:alexandria
                #:when-let
                #:assoc-value)
  (:import-from #:screenshotbot/model/image
                #:with-tmp-image-file
                #:make-image)
  (:import-from #:screenshotbot/model/company
                #:company)
  (:import-from #:screenshotbot/model/figma
                #:update-figma-link)
  (:import-from #:util/form-errors
                #:with-form-errors
                #:with-error-builder)
  (:import-from #:util/events
                #:push-event))
(in-package :screenshotbot/figma/view)

(named-readtables:in-readtable markup:syntax)

;; TODO: take in image etc.
(defun associate-figma (&key channel screenshot-name
                          redirect)
  "REDIRECT is where we want to redirect to after the change is made."
  (push-event :associate-figma)
  (let ((submit (nibble (url)
                  (validate-input
                   url
                   :channel channel
                   :screenshot-name screenshot-name
                   :redirect redirect))))
    <simple-card-page form-action= submit >
      <div class="form-group mb-3">
        <label for="figma-url" class="form-label">Figma Component URL</label>
        <input type="url" 
               class="form-control" 
               id="figma-url" 
               name="url" 
               placeholder="https://www.figma.com/file/..." 
               required />
        <div class="form-text">
          Enter the URL of the Figma component or frame you want to associate
        </div>
      </div>
      <button type="submit" class="btn btn-primary">Associate with Figma</button>
    </simple-card-page>))

(defun validate-input (url &key channel screenshot-name redirect)
  (with-error-builder (:check check :errors errors
                       :form-builder
                       (associate-figma :channel channel :screenshot-name screenshot-name
                                        :redirect redirect)
                       :form-args (:url url)
                       :success
                       (%start-oauth-flow
                        url
                        (lambda (image-url)
                          (perform-update-image-link :channel channel
                                                     :screenshot-name screenshot-name
                                                     :figma-url url
                                                     :image-url image-url
                                                     :redirect redirect))))

    (check :url (ignore-errors (quri:uri url))
           "Could not parse URL")
    (when-let ((uri (quri:uri url)))
      (check :url (or
                   (equal "www.figma.com" (quri:uri-host uri))
                   (equal "figma.com" (quri:uri-host uri)))
             "Must be a figma.com URL. Click on the component in Figma, and copy paste the URL here."))))

(defun make-image-from-data (image-data &key company)
  "Create an IMAGE instance from binary image data"
  (with-tmp-image-file (:stream stream :direction :output 
                        :element-type '(unsigned-byte 8)
                        :pathname temp-file)
    (write-sequence image-data stream)
    (finish-output stream)
    (make-image :pathname temp-file
                :company company)))

(defun perform-update-image-link (&key channel screenshot-name figma-url image-url
                                    redirect)
  "Downloads the Figma image and creates/updates the figma-link record"
  (let* ((image-data (download-figma-image image-url))
         (image (make-image-from-data image-data
                                      :company (company channel)))
         (figma-link (update-figma-link
                      :channel channel
                      :screenshot-name screenshot-name
                      :url figma-url
                      :image image)))
    (declare (ignore figma-link))
    (hex:safe-redirect
     (nibble ()
       (summary-screen :image image :redirect redirect)))))

(defun summary-screen (&key image redirect)
  <simple-card-page>
    <div class="text-center">
      <h3>Linked Figma component</h3>
      <div class="mt-4">
        <img src= (screenshotbot/model/image:image-public-url image)
             class="img-fluid"
             alt="Figma Component" />
      </div>
    </div>

    <div class= "card-footer">
      <a href=redirect class= "btn btn-primary" >Go back</a>
    </div>
  </simple-card-page>)



(defun %start-oauth-flow (figma-url callback)
  "Initiates the Figma OAuth flow to get access to the user's Figma files"
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
                         callback
                         :code code
                         :client-id client-id
                         :client-secret client-secret))
                       (t
                        (error "Invalid OAuth callback state")))))
         (state (nibble:nibble-id redirect))
         (oauth-url (format nil "https://www.figma.com/oauth?client_id=~a&redirect_uri=~a&scope=~a&state=~a&response_type=code"
                           (quri:url-encode client-id)
                           (quri:url-encode redirect-uri)
                           (quri:url-encode "file_content:read")
                           state)))
    (hex:safe-redirect oauth-url)))

(defun parse-figma-url (url)
  "Parse a Figma URL to extract file-key and node-id.
   Handles file, board, and design URLs:
   - https://www.figma.com/file/ABC123/Design-Name?node-id=1%3A2
   - https://www.figma.com/board/ABC123/Board-Name?node-id=1%3A2
   - https://www.figma.com/design/ABC123/Design-Name?node-id=1%3A2"
  (let ((parsed (quri:uri url)))
    (when (and (string= (quri:uri-host parsed) "www.figma.com")
               (or (str:starts-with-p "/file/" (quri:uri-path parsed))
                   (str:starts-with-p "/board/" (quri:uri-path parsed))
                   (str:starts-with-p "/design/" (quri:uri-path parsed))))
      (let* ((path-parts (str:split "/" (quri:uri-path parsed)))
             (file-key (third path-parts)) ; /file/FILE_KEY/... or /board/FILE_KEY/... or /design/FILE_KEY/...
             (query-params (quri:uri-query-params parsed))
             (node-id (cdr (assoc "node-id" query-params :test #'string=))))
        (when (and file-key node-id)
          (values file-key
                  (str:replace-all "-" ":" (quri:url-decode node-id))))))))


(defun fetch-figma-image (access-token file-key node-id)
  "Fetch image URL from Figma API for given file and node"
  (declare (optimize (debug 3) (speed 0)))
  (let* ((api-url (format nil "https://api.figma.com/v1/images/~A" file-key))
         (params `(("ids" . ,node-id)
                   ("format" . "png")))
         (headers `(("Authorization" . ,(format nil "Bearer ~A" access-token)))))
    
    (multiple-value-bind (body status-code)
        (util/request:http-request api-url
                                   :method :get
                                   :parameters params
                                   :additional-headers headers
                                   :want-string t)
      (if (= status-code 200)
          (let* ((json-response (let ((json:*json-identifier-name-to-lisp* #'identity))
                                  (json:decode-json-from-string body)))
                 (images (assoc-value json-response "images" :test #'string-equal))
                 (image-url (assoc-value images node-id :test #'string-equal)))
            (if image-url
                image-url
                (hex:safe-redirect
                 (nibble ()
                   <simple-card-page>
                     <p>
                       Oops, failed to find that image. Please make sure you copy pasted the URL correctly.
                     </p>
                   </simple-card-page>))))
          (error "Figma API error: ~A" status-code)))))

(defun handle-after-oauth-response (figma-url callback &key code client-secret client-id)
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
    (process-figma-url-for-screenshot
     figma-url
     callback
     access-token)))

(defun download-figma-image (image-url)
  "Download the actual image from Figma's CDN"
  (multiple-value-bind (image-data status-code)
      (drakma:http-request image-url :want-stream nil)
    (if (= status-code 200)
        image-data
        (error "Failed to download image: ~A" status-code))))

(defun process-figma-url-for-screenshot (figma-url callback access-token)
  "Complete flow: parse URL, fetch from API, download image"
  (multiple-value-bind (file-key node-id)
      (parse-figma-url figma-url)
    (unless (and file-key node-id)
      (error "Invalid Figma URL format"))
    
    (let* ((image-url (fetch-figma-image access-token file-key node-id)))
      (funcall callback image-url))))





