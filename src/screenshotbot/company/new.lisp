;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(uiop:define-package :screenshotbot/company/new
  (:use #:cl
        #:alexandria
        #:markup
        #:screenshotbot/user-api
        #:screenshotbot/model/company
        #:screenshotbot/model/user
        #:screenshotbot/invite)
  (:import-from #:screenshotbot/server
                #:home-url
                #:redirect-home
                #:defhandler)
  (:import-from #:bknr.datastore
                #:with-transaction)
  (:import-from #:screenshotbot/login/populate
                #:populate-company)
  (:import-from #:hex #:make-url)
  (:import-from #:util/form-errors
                #:with-form-errors)
  (:import-from #:core/ui/simple-card-page
                #:simple-card-page)
  (:import-from #:screenshotbot/events
                #:push-event)
  (:import-from #:screenshotbot/dashboard/ensure-company
                #:post-new-company))
(in-package :screenshotbot/company/new)

(markup:enable-reader)

(deftag organization-new-page ()
  <simple-card-page form-action= "/organization/new" >
    <div class= "card-header">
      <h4>Create a new Organization</h4>
    </div>
    <div class= "form-group">
      <label for= "org-name">Organization Name</label>
      <input type= "text" class= "form-control" name= "name" placeholder= "Xyz Inc." />
    </div>

    <!--
      <div class= "form-group">
        <label for= "org-id">Organization Id</label>
        <input type= "text" class= "form-control" placeholder= "xyzinc (Unique identifier)" />
      </div>
      -->

    <div class= "form-check small">
      <input type= "checkbox" class= "form-check-input" id= "i-agree" name= "i-agree" />
      <label for= "i-agree" class= "form-check-label" >Billing for this Organization will be on your billing information for your personal account. Depending on your plan, you might be charged for each user (apart from you) that will be added to this organization. We can transfer billing to another account on request.</label>
    </div>
    <div class= "card-footer">
      <input type= "submit" class= "btn btn-success form-control" value= "Create" >
    </div>
  </simple-card-page>)

(defhandler (nil :uri "/organization/new" :method :get) ()
  (let ((user (current-user)))
   (cond
     (nil ;; (not (professionalp user))
      <simple-card-page>
        <p>In order to create an organization, please upgrade to the Professional plan.</p>
        <a href= "/upgrade" type= "button" class= "btn btn-success">Upgrade</a>
      </simple-card-page>)
     (t
      <organization-new-page />))))

(defhandler (nil :uri "/organization/new" :method :post) (name i-agree)
   (post-new-company name i-agree
      :form #'organization-new-page
      :redirect 'company-create-confirmation-page))

(defhandler (company-create-confirmation-page
             :uri "/organizations/confirmed") ()
  <simple-card-page>
    <div class= "card-header">
      <h3>Your organization has been created</h3>
    </div>

    <p> Next, invite your coworkers and collaborators to this organization.</p>

    <div class= "card-footer">
      <a href= (make-url 'invite-page) class= "btn btn-primary">
        <i class= "uil uil-user-plus" />
        Invite Members
      </a>
      <a href= (home-url) class= "btn btn-secondary">
        <i class= "mdi mdi-home" />
        Home
      </a>

    </div>
  </simple-card-page>)


(defun company-switch-page (company)
  (check-type company company)
  (can-view! company)
  (setf (current-company) company)
  (push-event "company.switched")
  (redirect-home))
