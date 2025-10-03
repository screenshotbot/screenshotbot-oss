;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/microsoft-teams/channel-card
  (:use #:cl)
  (:import-from #:markup
                #:deftag)
  (:import-from #:nibble
                #:nibble)
  (:import-from #:screenshotbot/template
                #:app-template)
  (:import-from #:core/ui/simple-card-page
                #:simple-card-page
                #:confirmation-page)
  (:import-from #:screenshotbot/user-api
                #:channel-name)
  (:import-from #:screenshotbot/microsoft-teams/model
                #:teams-workflow
                #:teams-workflows-for-channel
                #:workflow-name)
  (:import-from #:screenshotbot/dashboard/channels
                #:microsoft-teams-card)
  (:import-from #:util/form-errors
                #:with-error-builder)
  (:import-from #:core/ui/mdi
                #:mdi))
(in-package :screenshotbot/microsoft-teams/channel-card)

(named-readtables:in-readtable markup:syntax)


(defmethod microsoft-teams-card (channel)
  <div class= "card mt-3">
    <div class= "card-body">
      <h4 class= "card-title mb-3">
        Microsoft Teams workflows
      </h4>

      ,(let ((workflows (fset:convert 'list (teams-workflows-for-channel channel))))
         (cond
           (workflows
            <div>
              <ul class= "list-group mb-3">
                ,@(loop for workflow in workflows
                        collect
                        <li class= "list-group-item d-flex justify-content-between align-items-center">
                          <span>,(workflow-name workflow)</span>
                          <a href= (nibble () (delete-workflow channel workflow))
                             class= "btn btn-sm btn-danger"
                             title= "Delete workflow">
                            <mdi name= "delete" /> Remove
                          </a>
                        </li>)
              </ul>
            </div>)
           (t
            <p class= "text-muted">No Teams workflows configured for this channel.</p>)))

      <div class= "d-flex gap-2">
        <a href= (nibble () (add-workflow channel)) class= "btn btn-primary">Add Workflow Webhook</a>
      </div>
    </div>
  </div>)

(defun add-workflow (channel)
  <simple-card-page title= (format nil "Add Workflow webhook for ~a" (channel-name channel))
                    form-action= (nibble (name url)
                                    (add-workflow/post channel :name name :url url))
                    >
    <div class= "card-header">
      <h5 class= "card-title mb-0">Add Workflow webhook for <tt>,(channel-name channel)</tt></h5>
    </div>
    <div class= "">
      <div class= "mb-3">
        <label for= "name" class= "form-label">Name</label>
        <input type= "text" class= "form-control" id= "name" name= "name" required= "true" />
      </div>
      
      <div class= "form-group mb-3">
        <label for= "url" class= "form-label">Webhook URL</label>
        <input type= "url" class= "form-control" id= "url" name= "url" required= "true" />
      </div>
    </div>
    <div class= "card-footer">
      <input class= "btn btn-primary" type= "submit" value= "Add" />
    </div>
  </simple-card-page>)

(defun add-workflow/post-validated (channel &key name url)
  (make-instance 'teams-workflow
                 :name name
                 :channel channel
                 :webhook-url url)
  (hex:safe-redirect
   "/channels/:id" :id (bknr.datastore:store-object-id channel)))

(defun add-workflow/post (channel &key name url)
  (with-error-builder (:check check
                       :form-builder (add-workflow channel)
                       :form-args (:name name :url url)
                       :errors errors
                       :success (add-workflow/post-validated channel :name name :url url))
    (check :name (>= (length name) 0)
           "Name can't be blank")
    (check :url (ignore-errors (quri:uri url))
           "Invalid URL")
    (check :url (str:ends-with-p "powerplatform.com" (quri:uri-host (quri:uri url)))
           "This doesn't look like a Microsoft teams URI")))

(defun delete-workflow (channel workflow)
  (confirmation-page
   :yes (nibble ()
          (bknr.datastore:delete-object workflow)
          (hex:safe-redirect
           "/channels/:id" :id (bknr.datastore:store-object-id channel)))
   :no (format nil "/channels/~a" (bknr.datastore:store-object-id channel))
   <div>
     <p>Are you sure you want to delete the workflow <strong>,(workflow-name workflow)</strong>?</p>
   </div>))



