(defpackage :screenshotbot/company/members
  (:use #:cl)
  (:import-from #:screenshotbot/settings-api
                #:settings-template
                #:defsettings)
  (:import-from #:screenshotbot/user-api
                #:user-companies
                #:current-user
                #:user-email
                #:user-full-name
                #:current-company)
  (:import-from #:screenshotbot/model/company
                #:company-admins
                #:company-owner
                #:company-invites)
  (:import-from #:screenshotbot/model/user
                #:users-for-company)
  (:import-from #:nibble
                #:nibble)
  (:import-from #:screenshotbot/model/invite
                #:invite-email)
  (:import-from #:bknr.datastore
                #:with-transaction)
  (:import-from #:screenshotbot/ui/confirmation-page
                #:confirmation-page)
  (:local-nicknames (#:a #:alexandria)))
(in-package :screenshotbot/company/members)

(markup:enable-reader)

(defsettings members-page
  :name "members"
  :section nil
  :title "Org Members"
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

(defun render-user-row (user company)
  (let* ((adminp (member user (company-admins company)))
         (can-delete-p
           (and
            (not adminp)
            (member (current-user) (company-admins company))))
         (delete (nibble ()
                   (assert can-delete-p)
                   (delete-user user company))))
    <tr class= "align-middle" >
      <td >,(user-full-name user)</td>
      <td>,(mailto (user-email user)) </td>
      <td>
        ,(cond
           (adminp
            "Admin")
           (t
            "Member"))
      </td>
      <td>
        <form>
          <input type= "submit" class="btn btn-danger" value= "Delete" formaction=delete disabled= (unless can-delete-p "disabled") />
        </form>
      </td>
    </tr>))

(defun delete-invite (invite company)
  (confirmation-page
   :yes (nibble ()
          (with-transaction ()
            (a:deletef (company-invites company)
                     invite))
          (hex:safe-redirect "/settings/members"))
   :no "/settings/members"
   <span>Delete invitation to ,(invite-email invite)?</span>))

(defun render-invite-row (invite company)
  (declare (ignore company))
  (let* ((delete (nibble ()
                   (delete-invite invite company))))
    <tr class= "align-middle" >
      <td><em>Unknown</em></td>
      <td>,(mailto (invite-email invite)) </td>
      <td>
        Invited
      </td>
      <td>
        <form>
          <input type= "submit" class="btn btn-danger" value= "Delete" formaction=delete  />
        </form>
      </td>
    </tr>))

(defun members-page ()
  (settings-template
   (let ((company (current-company)))
     <div class= "card card-body shadow border-0 table-wrapper table-responsive mt-3" >

       <table class="table user-table table-hover align-items-center" >
         <thead>
           <tr>
             <th>Name</th>
             <th>Email</th>
             <th>Status</th>
             <th></th>
           </tr>
         </thead>

         <tbody>
           ,@ (loop for invite in (company-invites company)
                    collect (render-invite-row invite company))
          ,@ (loop for user in (users-for-company company)
                   collect (render-user-row user company))
         </tbody>

       </table>

     </div>)))
