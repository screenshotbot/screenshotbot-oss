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
                #:user-companies
                #:current-user
                #:user-email
                #:user-full-name
                #:current-company)
  (:import-from #:screenshotbot/model/company
                #:company-admin-p
                #:company-admins
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
                #:confirmation-page)
  (:import-from #:core/ui/taskie
                #:taskie-list
                #:taskie-row
                #:taskie-page-title)
  (:import-from #:screenshotbot/template
                #:mdi)
  (:import-from #:auth/model/invite
                #:all-unused-invites)
  (:local-nicknames (#:a #:alexandria)))
(in-package :screenshotbot/company/members)

(markup:enable-reader)

(defsettings members-page
  :name "members"
  :section :organization
  :title "Members"
  :handler (lambda ()
             (members-page)))
(defun mailto (x)
  <a href= (format nil "mailto:~a" x)>,(progn x)</a>)

(defun delete-user (user company &aux (back "/settings/members"))
  (assert (not (member user (company-admins company))))
  (confirmation-page
   :yes (nibble ()
          (with-transaction ()
            (a:deletef
             (user-companies user)
             company)
            (hex:safe-redirect back)))
   :no back
   <p>Remove ,(user-full-name user) from this Organization?</p>))

(markup:deftag delete-button (&key action)
  <form>
    <button type= "submit" class="btn btn-link" value= "Delete" formaction=action >
      <mdi name= "delete" class= "text-danger" />
    </button>
  </form>)

(defun render-user-row (user company)
  (let* ((adminp (company-admin-p company user))
         (can-delete-p
           (and
            (not adminp)
            (member (current-user) (company-admins company))))
         (delete (nibble ()
                   (assert can-delete-p)
                   (delete-user user company))))
    <taskie-row>
      <span><img class= "rounded-circle avatar" src= (user-image-url user) /></span>
      <span >,(user-full-name user)</span>
      <span>,(mailto (user-email user)) </span>
      <span>
        ,(cond
           (adminp
            "Admin")
           (t
            "Member"))
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

(defun members-page ()
  (settings-template
   (let ((company (current-company)))

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


       ,(taskie-page-title :title "Members" :class "pt-3")

       ,(taskie-list
         :headers '("" "Name" "Email" "Status" "Actions")
         :class "with-avatar"
         :items (users-for-company company)
         :checkboxes nil
         :empty-message "No users"
         :row-generator (lambda (user)
                          (render-user-row user company)))

     </markup:merge-tag>

)))
