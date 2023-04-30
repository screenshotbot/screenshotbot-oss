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
    <div class= "container body-vh-100" style= "max-width: 40em" >
      <div class= "row h-100">
        <div class= "my-auto" style= "padding-bottom: 20vh;" >
          <h4 class= "mb-3" >
            Create Organization
          </h4>
             <p class= "mb-3" >Before we can begin, let's create an organization for your
               projects. Organizations also let you collaborate with other users.</p>

           <div class= "form-group pt-3">

             <label for= "company" class= "form-label text-muted">
               Organization name
             </label>
             <input type= "text" placeholder= "My company, Inc."
                    name= "company"
                    id= "company"
                    class= "form-control" />
           </div>

           <div class= "form-group mt-3">
             <input type= "submit" class= "btn btn-primary form-control"
                    value= "Create and continue" />
           </div>

        </div>
      </div>
       </div>
     </dashboard-template>)
