(defpackage :screenshotbot/company/members
  (:use #:cl)
  (:import-from #:screenshotbot/settings-api
                #:settings-template
                #:defsettings)
  (:import-from #:screenshotbot/user-api
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

(defun delete-user (user company)
  "unimplemented")

(defun render-user-row (user company)
  (let* ((adminp (member user (company-admins company)))
         (delete (nibble ()
                   (assert (not adminp))
                   (delete-user user company))))
    <tr class= "align-middle" >
      <td >,(user-full-name user)</td>
      <td>,(mailto (user-email user)) </td>
      <td> ,(cond
              (adminp
               "Admin")
              (t
               "Member")) </td>
      <td>
        <form>
          <input type= "submit" class="btn btn-danger" value= "Delete" formaction=delete disabled= (when adminp "disabled") />
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
          ,@ (loop for user in (users-for-company company)
                   collect (render-user-row user company))
         </tbody>

       </table>

     </div>)))
