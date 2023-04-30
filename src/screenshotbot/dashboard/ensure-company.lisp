;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/dashboard/ensure-company
  (:use #:cl)
  (:import-from #:screenshotbot/user-api
                #:user-companies)
  (:import-from #:screenshotbot/installation
                #:call-with-ensure-user-prepared
                #:one-owned-company-per-user)
  (:import-from #:screenshotbot/template
                #:dashboard-template
                #:app-template))
(in-package :screenshotbot/dashboard/ensure-company)

(named-readtables:in-readtable markup:syntax)

(defmethod call-with-ensure-user-prepared ((installation one-owned-company-per-user)
                                           user
                                           fn)
  (cond
    ((user-companies user)
     (funcall fn))
    (t
     (%new-company))))

(defun %new-company ()
       <dashboard-template left-nav-bar=nil >
       <div class= "container" style= "max-width: 50em" >
         <div class= "card mt-3">
           <div class= "card-header">
           <h3 >
             New Organization
           </h3>

           </div>
         <div class= "card-body">
           <div class= "form-group">
             <p>Before we can begin, let's create an organization for your
               projects. Organizations also let you invite colleagues and collaborators.</p>
             <label for= "company" class= "form-label text-muted">
               Organization name
             </label>
             <input type= "text" placeholder= "My company, Inc."
                    name= "company"
                    id= "company"
                    class= "form-control" />
           </div>
         </div>

         <div class= "card-footer">
           <input type= "submit" class= "btn btn-primary" value= "Get Started" />
         </div>

       </div>
       </div>
     </dashboard-template>)
