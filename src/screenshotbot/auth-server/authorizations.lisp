;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/auth-server/authorizations
  (:use #:cl)
  (:import-from #:core/ui/mdi
                #:mdi)
  (:import-from #:core/ui/simple-card-page
                #:confirmation-page)
  (:import-from #:core/ui/taskie
                #:taskie-list
                #:taskie-page-title
                #:taskie-row)
  (:import-from #:core/ui/template
                #:app-template)
  (:import-from #:nibble
                #:nibble)
  (:import-from #:screenshotbot/auth-server/model
                #:grant-client
                #:grant-created-at
                #:grant-revoked-p
                #:grant-scopes
                #:grant-user
                #:oauth-client-name
                #:oauth-client-id
                #:oauth-grants-for-company
                #:revoke-grant)
  (:import-from #:screenshotbot/auth-server/scopes
                #:find-scope
                #:scope-label)
  (:import-from #:screenshotbot/login/common
                #:with-login)
  (:import-from #:screenshotbot/server
                #:defhandler)
  (:import-from #:util/timeago
                #:timeago)
  (:export
   #:active-grants)
  (:documentation "The page where a user sees, and disconnects, the applications
they have authorized over OAuth."))
(in-package :screenshotbot/auth-server/authorizations)

(named-readtables:in-readtable markup:syntax)

(defun active-grants (company)
  (sort
   (remove-if #'grant-revoked-p
              (fset:convert 'list (oauth-grants-for-company company)))
   #'>
   :key #'grant-created-at))

(defun %confirm-revoke (grant)
  (confirmation-page
   :danger t
   :yes (nibble ()
          (revoke-grant grant)
          (hex:safe-redirect "/oauth/authorizations"))
   :no (nibble ()
         (hex:safe-redirect "/oauth/authorizations"))
   <p>Disconnect ,(or (oauth-client-name (grant-client grant))
                      (oauth-client-id (grant-client grant)))?
     It will immediately stop being able to access your account.</p>))

(defun %scope-summary (grant)
  (str:join ", "
            (loop for name in (grant-scopes grant)
                  for scope = (find-scope name)
                  if scope
                    collect (scope-label scope))))

(defun %authorizations-page (&key (company (auth:current-company)))
  (auth:can-view! company)
  (let ((grants (active-grants company)))
    <app-template title= "Screenshotbot: Authorized Applications" >
      <taskie-page-title title= "Authorized applications" />

      ,(taskie-list
        :items grants
        :headers (list "Application" "Access" "User" "Authorized" "Actions")
        :empty-message "You haven't connected any applications yet"
        :checkboxes nil
        :row-generator
        (lambda (grant)
          (let ((revoke (nibble ()
                          (%confirm-revoke grant))))
            <taskie-row>
              <span>,(or (oauth-client-name (grant-client grant))
                         (oauth-client-id (grant-client grant)))</span>
              <span class= "d-inline-block text-truncate" style= "max-width: 24em"
                    title= (%scope-summary grant) >
                ,(%scope-summary grant)
              </span>
              <span>,(auth:user-email (grant-user grant))</span>
              <span>,(timeago :timestamp (grant-created-at grant))</span>
              <span>
                <form style= "display:inline-block" method= "post" >
                  <button type= "submit" formaction=revoke formmethod= "post"
                          class= "btn btn-link" value= "Disconnect" >
                    <mdi name= "delete" class= "text-danger" />
                  </button>
                </form>
              </span>
            </taskie-row>)))
    </app-template>))

(defhandler (nil :uri "/oauth/authorizations" :method :get) ()
  (with-login ()
    (%authorizations-page)))
