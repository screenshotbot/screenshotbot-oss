;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/dashboard/microsoft-teams
  (:use #:cl)
  (:import-from #:markup
                #:deftag)
  (:import-from #:nibble
                #:nibble)
  (:import-from #:screenshotbot/template
                #:app-template)
  (:import-from #:core/ui/simple-card-page
                #:simple-card-page)
  (:import-from #:screenshotbot/user-api
                #:channel-name)
  (:import-from #:screenshotbot/microsoft-teams/model
                #:teams-workflow))
(in-package :screenshotbot/dashboard/microsoft-teams)

(named-readtables:in-readtable markup:syntax)


(deftag microsoft-teams-card (&key channel)
  <div class= "card mt-3">
    <div class= "card-body">
      <h4 class= "card-title mb-3">
        Microsoft Teams worfklows
      </h4>

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

(defun add-workflow/post (channel &key name url)
  (make-instance 'teams-workflow
                 :name name
                 :webhook-url url)
  (hex:safe-redirect
   "/channels/:id" :id (bknr.datastore:store-object-id channel)))



