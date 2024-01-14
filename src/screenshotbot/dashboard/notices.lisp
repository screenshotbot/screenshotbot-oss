;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/dashboard/notices
  (:use :cl)
  (:import-from #:auth
                #:current-user)
  (:import-from #:bknr.datastore
                #:store-object-id
                #:with-transaction)
  (:import-from #:markup/markup
                #:deftag
                #:unescaped)
  (:import-from #:nibble
                #:nibble)
  (:import-from #:screenshotbot/invite
                #:accept-invite)
  (:import-from #:screenshotbot/notice-api
                #:invite-company
                #:notice-summary
                #:notice-title)
  (:import-from #:screenshotbot/server
                #:defhandler)
  (:import-from #:screenshotbot/template
                #:user-notice-list)
  (:import-from #:screenshotbot/user-api
                #:company-name
                #:unaccepted-invites
                #:user
                #:user-notices)
  (:import-from #:util/store/object-id
                #:oid))
(in-package :screenshotbot/dashboard/notices)

(markup:enable-reader)

(deftag user-notice-list (&key (user (current-user)))
  <div id= "user-notice-list" class= "row">
    <!-- user notice list-->
    ,@ (loop for invite in (unaccepted-invites user) collect
    <div class= "col-md-4 mt-2">
      <form action= (util:copying (invite)  (nibble () (accept-invite invite))) method= "POST" >
        <div class= "card">
          <div class= "card-header">
            <h3>,(company-name (invite-company invite)) </h3>
          </div>
          <input type= "hidden"
                 name= "invite-id"
                 value= (store-object-id invite) />
          <div class= "card-body">
            <p>You've been invited to collaborate in this Organization. </p>
          </div>

          <div class= "card-footer">
            <input type= "submit" class= "btn btn-success" value= "Accept Invitation" />
          </div>
        </div>
      </form>
    </div>)
       ,@ (loop for notice in (user-notices user) collect
                (render-notice notice))
  </div>)

(defmethod render-notice (notice)
  <div class= "col-md-4 mt-2">
    <div class= "card">
      <div class= "card-header pt-0 pb-0">
        <h3>,(notice-title notice)</h3>
      </div>

      <div class= "card-body pt-0 pb-0">
        <p>,(notice-summary notice)</p>
      </div>

      <div class= "card-footer">
        <input type= "button" class= "btn btn-danger user-notice-dismiss" value= "Dismiss"
               data-notice-id= (oid notice) />
      </div>
    </div>
  </div>)

(defhandler (nil :uri "/notice/dismiss" :method :post) (notice-id)
  (with-transaction ()
    (setf (user-notices (current-user))
          (remove-if (lambda (x)
                       (equal notice-id
                              (oid x)))
                     (user-notices (current-user)))))
  <user-notice-list />)
