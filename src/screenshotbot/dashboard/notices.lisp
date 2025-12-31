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

(named-readtables:in-readtable markup:syntax)

(deftag user-notice-list (&key (user (current-user)))
  <div id= "user-notice-list" >
    <!-- user notice list-->
    ,@ (loop for invite in (unaccepted-invites user) collect
                                                     (render-invite invite))
    ,@ (loop for notice in (user-notices user) collect
                                               (render-notice notice))
  </div>)

(deftag notice-toast (children)
  <div class="toast show position-fixed bottom-0 end-0 m-3 " role="alert" aria-live="assertive" aria-atomic="true" data-bs-autohide="false">
    ,@children
  </div>)

(defmethod render-invite (invite)
  <notice-toast>
    <div class="toast-header">
      <strong class="me-auto">Invitation to ,(company-name (invite-company invite))</strong>
      <button type="button" class="btn-close" data-bs-dismiss="toast" aria-label="Close"></button>
    </div>
    <div class="toast-body">
      <p>You've been invited to collaborate in this Organization.</p>
      <form action= (util:copying (invite)  (nibble () (accept-invite invite))) method= "POST" >
        <input type= "hidden"
               name= "invite-id"
               value= (store-object-id invite) />
        <input type= "submit" class= "btn btn-success" value= "Accept Invitation" />
      </form>
    </div>
  </notice-toast>)

(defmethod render-notice (notice)
  <notice-toast>
    <div class="toast-header">
      <strong class="me-auto">,(notice-title notice)</strong>
      <button type="button" class="btn-close user-notice-dismiss" data-bs-dismiss="toast" aria-label="Close" data-notice-id=(oid notice)></button>
    </div>
    <div class="toast-body">
      ,(notice-summary notice)
    </div>
  </notice-toast>)

(defhandler (nil :uri "/notice/dismiss" :method :post) (notice-id)
  (with-transaction ()
    (setf (user-notices (current-user))
          (remove-if (lambda (x)
                       (equal notice-id
                              (oid x)))
                     (user-notices (current-user)))))
  <user-notice-list />)
