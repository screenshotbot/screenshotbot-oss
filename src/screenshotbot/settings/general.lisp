;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(uiop:define-package :screenshotbot/settings/general
  (:use #:cl
        #:alexandria
        #:markup
        #:screenshotbot/model/user
        #:screenshotbot/user-api
        #:nibble
        #:screenshotbot/settings/settings-template)
  (:import-from #:bknr.datastore
                #:with-transaction)
  (:import-from #:screenshotbot/login/signup
                #:validate-name)
  (:import-from #:util/form-errors
                #:with-form-errors))
(in-package :screenshotbot/settings/general)

(markup:enable-reader)

(defun %general-settings ()
  (let ((submit (nibble (full-name)
                  (submit-form :full-name full-name))))
    <settings-template>
      <form method="post" action=submit >
        <div class= "card mt-3">
          <div class= "card-header">
            <h4>Account Details</h4>
          </div>
          <div class= "card-body">
            <div class= "row">
              <div class= "col-md-6">
                <div class= "setting-item-title" >Name</div>
                <div class= "setting-item-subtitle" >Your full name</div>
              </div>
              <div class= "col-md-6">
                <div class= "form-group setting-form-group">
                  <div class= "row">
                    <input type= "text" name= "full-name" class= "form-control"
                           value= (user-full-name (current-user)) />
                  </div>
                </div>
              </div>
            </div>
          </div> <!-- /card-body -->
          <div class= "card-footer">
            <input type= "submit" class= "btn btn-primary" value= "Save changes" />
          </div>

        </div>
      </form>
    </settings-template>))

(defsettings general-settings
  :name "general"
  :title "General"
  :section nil
  :handler '%general-settings)

(defun submit-form (&key full-name)
  (let ((errors))
   (flet ((check (name test message)
            (unless test
              (push (cons name message) errors))))
     (validate-name #'check full-name)
     (cond
       (errors
        (with-form-errors (:full-name full-name
                           :errors errors
                           :was-validated t)
          (%general-settings)))
       (t
        (setf (user-full-name (current-user))
              full-name)
        (hex:safe-redirect "/settings/general"))))))
