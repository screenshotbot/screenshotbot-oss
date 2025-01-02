;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/company/members
  (:use #:cl)
  (:import-from #:screenshotbot/settings-api
                #:settings-template
                #:defsettings)
  (:import-from #:screenshotbot/user-api
                #:user-image-url
                #:current-user
                #:user-email
                #:user-full-name
                #:current-company)
  (:import-from #:screenshotbot/model/company
                #:company-admin-p
                #:company-owner)
  (:import-from #:screenshotbot/model/user
                #:users-for-company)
  (:import-from #:nibble
                #:nibble)
  (:import-from #:screenshotbot/model/invite
                #:invite-email)
  (:import-from #:bknr.datastore
                #:with-transaction)
  (:import-from #:core/ui/simple-card-page
                #:simple-card-page
                #:confirmation-page)
  (:import-from #:core/ui/taskie
                #:taskie-list
                #:taskie-row
                #:taskie-page-title)
  (:import-from #:screenshotbot/template
                #:app-template
                #:mdi)
  (:import-from #:auth/model/invite
                #:all-unused-invites)
  (:import-from #:screenshotbot/invite
                #:invite-enabled-p)
  (:local-nicknames (#:a #:alexandria)
                    (#:roles #:auth/model/roles)))
(in-package :screenshotbot/company/members)

(named-readtables:in-readtable markup:syntax)

(defsettings members-page
  :name "members"
  :section :organization
  :title "Members"
  :handler (lambda ()
             (members-page)))
(defun mailto (x)
  <a href= (format nil "mailto:~a" x)>,(progn x)</a>)

(defun delete-user (user company &aux (back "/settings/members"))
  (assert (not (roles:has-role-p company user 'roles:admin)))
  (confirmation-page
   :yes (nibble ()
          (setf (roles:user-role company user) nil)
          (hex:safe-redirect back))
   :no back
   <p>Remove ,(user-full-name user) from this Organization?</p>))

(markup:deftag delete-button (&key action)
  <form>
    <button type= "submit" class="btn btn-link" value= "Delete" formaction=action >
      <mdi name= "delete" class= "text-danger" />
    </button>
  </form>)

(defun can-change-user-role-p (company user)
  (and
   (not (eql user (auth:current-user)))
   (roles:has-role-p company (auth:current-user) 'roles:admin)
   (not (roles:has-role-p company user 'roles:owner))))

(defvar *allowed-roles*
  (list
   'roles:admin
   'roles:standard-member
   'roles:external-member))

(defun %save-role (&key user company role)
  (assert (can-change-user-role-p company user))
  (let ((role (loop for r in *allowed-roles*
                    if (string-equal r role)
                      return r)))
    (assert role)
    (setf (roles:user-role company user)
          role)
    (hex:safe-redirect "/settings/members")))

(defun %edit-role (&key user company)
  (assert (can-change-user-role-p company user))
  (let ((submit (nibble (role :method :post) (%save-role
                                              :company company
                                              :user user
                                              :role role))))
    <simple-card-page  form-action= submit  >
      <div class= "card-body">
        <label for= "role-selector" class= "form-label">Choose role for ,(auth:user-email user):</label>
        <select class= "form-select mb-2" id= "role-selector" name= "role" >
          ,@ (loop for role in *allowed-roles*
                   for selected = (typep (roles:user-role company user) role)
                   collect
                   <option value= (string role)
                           selected= (when selected "selected") >
                     ,(roles:role-friendly-name (make-instance role))
                   </option>)
        </select>

      </div>

      <div class= "card-footer">
        <input type= "submit" class= "btn btn-primary" value= "Change role" />
        <a href= "/settings/members" class= "btn btn-secondary" >Cancel</a>
      </div>
    </simple-card-page>))

(defun render-user-row (user company)
  (let* ((adminp (company-admin-p company user))
         (can-delete-p
           (and
            (not adminp)
            (company-admin-p company (current-user))))
         (delete (nibble ()
                   (assert can-delete-p)
                   (delete-user user company)))
         (role (roles:user-role company user)))
    <taskie-row>
      <span><img class= "rounded-circle avatar" src= (user-image-url user) /></span>
      <span >,(user-full-name user)</span>
      <span>,(mailto (user-email user)) </span>
      <span>
        ,(roles:role-friendly-name role)
        ,(when (can-change-user-role-p company user)
           <a href= (nibble () (%edit-role :user user :company company)) title= "Edit role">
             <mdi name= "edit" class= "ps-1" />
           </a>)
      </span>
      <span>
        ,(when can-delete-p
           <delete-button action=delete />)
      </span>
    </taskie-row>))

(defun delete-invite (invite company)
  (confirmation-page
   :yes (nibble ()
          (with-transaction ()
            (bknr.datastore:delete-object invite))
          (hex:safe-redirect "/settings/members"))
   :no "/settings/members"
   <span>Delete invitation to ,(invite-email invite)?</span>))


(defun render-invite-row (invite company)
  (let* ((delete (nibble ()
                   (delete-invite invite company))))
    <taskie-row>
      <span><em>Unknown</em></span>
      <span>,(mailto (invite-email invite)) </span>
      <span>
        Invited
      </span>
      <span>
        <delete-button action=delete />
      </span>
    </taskie-row>))

(defun invites-section (company)
  <markup:merge-tag>
    ,(taskie-page-title
      :title "Pending Invites"
      <a href= "/invite" class= "btn btn-success btn-sm">Invite Member</a>)
    ,(taskie-list
      :headers '("Name" "Email" "Status" "Actions")
      :items (all-unused-invites :company company)
      :checkboxes nil
      :empty-message "No pending invites"
      :row-generator (lambda (invite)
                       (render-invite-row invite company)))
  </markup:merge-tag>)

(defun members-page ()
  (settings-template
   (let ((company (current-company)))

     <markup:merge-tag>
       ,(when (invite-enabled-p company)
          (invites-section company))

       ,(taskie-page-title :title "Members" :class "pt-3")

       ,(taskie-list
         :headers '("" "Name" "Email" "Status" "Actions")
         :class "with-avatar"
         :items (remove-if
                 (lambda (user)
                   (or
                    (not user)
                    (roles:has-role-p company user 'roles:hidden-user)))
                 (roles:users-for-company company))
         :checkboxes nil
         :empty-message "No users"
         :row-generator (lambda (user)
                          (render-user-row user company)))

     </markup:merge-tag>

)))
