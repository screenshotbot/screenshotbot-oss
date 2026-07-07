;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/github/github-app-settings
  (:use #:cl)
  (:import-from #:core/ui/simple-card-page
                #:simple-card-page)
  (:import-from #:nibble
                #:nibble)
  (:import-from #:util/form-errors
                #:with-error-builder)
  (:import-from #:screenshotbot/github/github-app
                #:github-app-id
                #:github-app-name
                #:transient-github-app
                #:update-github-app
                #:github-app)
  (:import-from #:screenshotbot/github/jwt-token
                #:github-create-jwt-token)
  (:import-from #:util/misc
                #:?.
                #:not-null!)
  (:export
   #:github-app-settings-form))
(in-package :screenshotbot/github/github-app-settings)

(named-readtables:in-readtable markup:syntax) 

(defun github-app-settings-form (github-app)
  (let ((action (nibble (app-id private-key :method :post)
                  (%submit :app-id app-id :private-key private-key :github-app github-app))))
    <simple-card-page form-action=action enctype="multipart/form-data" >
      <div class= "card-header">
        <h3>Update GitHub app id</h3>
      </div>
      <div class= "mb-3">
        <label for= "app-id" class= "form-label" >GitHub App ID</label>
        <input type= "text" name= "app-id" id= "app-id" class= "form-control" placeholder= "111111"
               value= (?. github-app-id github-app ) />
      </div>

      <div class= "mb-3">
        <label for= "private-key" class= "form-label" >Private Key</label>
        <input type= "file" class= "form-control" name= "private-key" id= "private-key" />
      </div>
      <div class= "card-footer">
        <input type= "submit" value= "Update" class= "btn btn-primary" />
        <a href= "/settings/github" class= "btn btn-secondary">Cancel</a>
      </div>
    </simple-card-page>))

(defun %submit (&key app-id private-key github-app)

  (with-error-builder (:check check
                       :errors errors
                       :form-builder (github-app-settings-form github-app)
                       :form-args (:app-id app-id)
                       :success (%update :app-id app-id :private-key private-key))
    (check :app-id
           (ignore-errors
            (parse-integer app-id))
           "App ID must be an integer")
    (check :private-key
           (= 3 (length private-key))
           "Private key must be provided")
    (assert (listp private-key))
    (when private-key
     (let* ((private-key-file (first private-key))
            (github-app (make-instance 'transient-github-app
                                       :app-id app-id
                                       :private-key (uiop:read-file-string private-key-file))))
       (check :private-key
              (ignore-errors
               (github-create-jwt-token
                :github-app github-app))
              "Could not read the private key, is that a correct PEM file provided by GitHub?")
       (check :app-id
              (ignore-errors
               ;; Hits the network
               (github-app-name github-app))
              "Could not validate the request, please check the App ID and private key and try again")))))

(defun %update (&key app-id private-key)
  (update-github-app
   :app-id app-id
   :private-key (uiop:read-file-string (first private-key))
   :company (not-null! (auth:current-company)))
  (hex:safe-redirect "/settings/github"))
