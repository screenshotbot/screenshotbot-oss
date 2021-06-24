(pkg:define-package :screenshotbot/dashboard/site-admin
    (:use #:cl
          #:alexandria
          #:nibble)
  (:import-from #:../template
                #:app-template)
  (:import-from #:../model/user
                #:adminp)
  (:import-from #:../user-api
                #:current-user)
  (:import-from #:../server
                #:defhandler
                #:with-login)
  (:import-from #:bknr.datastore
                #:with-transaction)
  (:import-from #:../model/core
                #:generate-api-secret)
  (:import-from #:../ui
                #:simple-card-page))

(markup:enable-reader)

(defun finish-self-promotion (secret-file)
  (cond
    ((path:-e secret-file)
     (delete-file secret-file)
     (with-transaction ()
       (setf (adminp (current-user)) t))
     <app-template>
       You're now a site admin!
     </app-template>)
    (t
     <app-template>
       <p> Couldn't find the file ,(progn (namestring secret-file))</p>
       <a class= "btn btn-secondary" href= (nibble () (%self-promotion :secret-file secret-file)) >
         Try Again
       </a>
     </app-template>
       )))

(defun %self-promotion (&key secret-file)
  (with-login ()
   (let* ((secret (generate-api-secret))
          (secret-file (or secret-file
                           (path:catfile
                            (asdf:system-relative-pathname :screenshotbot "../../")
                            (format nil "ADMIN-PRIVILEGES-~a" secret)))))
     <simple-card-page>

       <div class= "card-header">
         <h4>Become a Site-Admin</h4>
       </div>

       <p>
         As the user <tt>,(uiop:getenv "USER")</tt>, run the following command:
       </p>

       <div class= "mt-4 mb-4">
         <code >
           touch ,(progn secret-file)
         </code>
       </div>

       <p class= "mt-2" >
         Once you have done that hit continue.
       </p>

       <div class= "card-footer">
         <a href= (nibble () (finish-self-promotion secret-file)) class= "btn btn-primary" >
           Continue
         </a>
       </div>

     </simple-card-page>)))

(defhandler (nil :uri "/site-admin/self-promotion") ()
  (%self-promotion))
