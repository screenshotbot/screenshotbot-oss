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
                #:defhandler)
  (:import-from #:bknr.datastore
                #:with-transaction)
  (:import-from #:screenshotbot/login/populate
                #:populate-company)
  (:import-from #:hex #:make-url)
  (:import-from #:util/form-errors
                #:with-form-errors)
  (:import-from #:screenshotbot/ui
                #:simple-card-page))
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
  (let ((errors))
    (flet ((check (name test message)
             (unless test
               (push (cons name message) errors))))
      (check :name (and name (>= (length name) 6))
             "Organization name must be at least 6 letters long")
      (check :i-agree i-agree
             "You must accept that you understood how billing works")
      (check :name
             (not (company-with-name  name))
             "There's already a company with that name. If you think
              you should be the appropriate admin for this company,
              please contact us.")
      (check :name
             (or (adminp (current-user))
                 (< (length (user-companies (current-user))) 4))
             "You have reached the limit of organizations you can
             create. We have this limit just to prevent abuse. Please
             contact us to create this organization for you."))
    (cond
      (errors
       (with-form-errors (:name name
                          :errors errors
                          :was-validated t)
         (organization-new-page )))
      (t
       (let* ((user (current-user))
              (company (make-instance 'company
                                      :name name
                                      :admins (list user)
                                      :owner user)))
         (with-transaction ()
           (push
            company
            (user-companies user)))
         (setf (current-company) company)
         (populate-company company)
         (hex:safe-redirect 'company-create-confirmation-page))))))

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
      <a href= "/" class= "btn btn-secondary">
        <i class= "mdi mdi-home" />
        Home
      </a>

    </div>
  </simple-card-page>)


(defun company-switch-page (company)
  (check-type company company)
  (can-view! company)
  (setf (current-company) company)
  (hex:safe-redirect "/"))
