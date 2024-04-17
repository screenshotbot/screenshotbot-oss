;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/dashboard/ensure-company
  (:use #:cl)
  (:import-from #:screenshotbot/user-api
                #:company-name
                #:user-full-name
                #:unaccepted-invites
                #:current-company
                #:current-user
                #:adminp
                #:user-companies)
  (:import-from #:screenshotbot/installation
                #:call-with-ensure-user-prepared
                #:one-owned-company-per-user)
  (:import-from #:screenshotbot/template
                #:dashboard-template
                #:app-template)
  (:import-from #:nibble
                #:nibble)
  (:import-from #:screenshotbot/model/company
                #:company
                #:company-with-name)
  (:import-from #:screenshotbot/events
                #:push-event)
  (:import-from #:bknr.datastore
                #:with-transaction)
  (:import-from #:util/form-errors
                #:with-form-errors)
  (:import-from #:screenshotbot/invite
                #:accept-invite)
  (:import-from #:screenshotbot/model/invite
                #:inviter)
  (:import-from #:screenshotbot/notice-api
                #:invite-company)
  (:export
   #:post-new-company))
(in-package :screenshotbot/dashboard/ensure-company)

(named-readtables:in-readtable markup:syntax)

(defmethod call-with-ensure-user-prepared ((installation one-owned-company-per-user)
                                           user
                                           fn)
  ;; If the user is not
  (cond
    (user
     (ensure-company-or-invite
      user
      fn
      (unaccepted-invites user)))
    (t
     (funcall fn))))

(defun ensure-company-or-invite (user fn invites)
  (cond
    ((user-companies user)
     (funcall fn))
    (invites
     (let ((invite (car invites)))
       (%accept-invite user invite
                       :accept (nibble ()
                                 (accept-invite invite)
                                 (hex:safe-redirect
                                  (nibble ()
                                    (funcall fn))))
                       :reject (nibble ()
                                (ensure-company-or-invite
                                 user fn (cdr invites))))))
    (t
     (%new-company :redirect (nibble () (funcall fn))
                   :user user))))

(markup:deftag center-box (children)
  <dashboard-template left-nav-bar=nil >
    <div class= "container body-vh-100" style= "max-width: 40em" >
      <div class= "row h-100">
        <div class= "my-auto" style= "padding-bottom: 20vh;" >
          ,@children
        </div>
      </div>
    </div>
  </dashboard-template>)

(defun %accept-invite (user invite &key accept reject)
  <center-box>
    <h4>You have an invitation</h4>

    <p>You have a pending invite from ,(user-full-name (inviter invite)) to join
      <b>,(company-name (invite-company invite))</b> </p>

    <form action=accept method= "post" class= "text-center" >
      <div class= "form-group mt-3">
        <input type= "submit" value= "Accept and continue" class= "btn btn-primary form-control" />
      </div>
      <div class= "mt-3">
        <a href= reject class= "mt-3" >Decline for now</a>
      </div>

    </form>
  </center-box>)

(defun %new-company (&key redirect
                       user)
  (let ((post (nibble (name)
                (post-new-company name t
                                  :user user
                                  :form (lambda ()
                                          (%new-company :redirect redirect
                                                        :user user))
                                  :redirect redirect))))
  <center-box>
    <h4 class= "mb-3" >
      Create Organization
    </h4>
    <p class= "mb-3" >Before we can begin, let's create an organization for your
      projects. Organizations also let you collaborate with other users.</p>

    <p class= "text-muted">You will be able to change this name later.</p>

    <form action=post method= "post" >
      <div class= "form-group pt-3">

        <label for= "name" class= "form-label text-muted">
          Organization name
        </label>
        <input type= "text" placeholder= "My company, Inc."
               name= "name"
               id= "name"
               class= "form-control" />
      </div>

      <div class= "form-group mt-3">
        <input type= "submit" class= "btn btn-primary form-control"
               value= "Create and continue" />
      </div>
    </form>
  </center-box>))

(defun post-new-company (name i-agree
                         &key form
                           (user (current-user))
                           redirect)
  "This is also called by /organization/new"
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
             (or (adminp user)
                 (< (length (user-companies user)) 4))
             "You have reached the limit of organizations you can
             create. We have this limit just to prevent abuse. Please
             contact us to create this organization for you."))
    (cond
      (errors
       (with-form-errors (:name name
                          :errors errors
                          :was-validated t)
         (funcall form)))
      (t
       (let* ((company (make-instance 'company
                                      :name name
                                      :admins (list user)
                                      :owner user)))
         (with-transaction ()
           (push
            company
            (user-companies user)))
         (setf (current-company) company)
         (push-event :company.new)
         (populate-company company)
         (hex:safe-redirect redirect))))))
