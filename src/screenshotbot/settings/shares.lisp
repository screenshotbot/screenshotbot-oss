;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/settings/shares
  (:use #:cl
        #:screenshotbot/settings/settings-template)
  (:import-from #:screenshotbot/settings-api
                #:defsettings)
  (:import-from #:screenshotbot/user-api
                #:current-user
                #:user-full-name
                #:current-company)
  (:import-from #:screenshotbot/model/sharing
                #:share-revoked-p
                #:expiry-date
                #:share-creator
                #:shares-for-company)
  (:import-from #:screenshotbot/dashboard/reports
                #:shared-report-page)
  (:import-from #:util/object-id
                #:oid-array)
  (:import-from #:screenshotbot/taskie
                #:taskie-list
                #:taskie-page-title
                #:taskie-row
                #:timeago)
  (:import-from #:nibble
                #:nibble)
  (:import-from #:bknr.datastore
                #:with-transaction)
  (:local-nicknames (#:a #:alexandria)))
(in-package :screenshotbot/settings/shares)

(markup:enable-reader)

(defsettings settings-shares-page
  :name "shares"
  :section nil
  :title "Public Shares"
  :handler 'shares-page)

(defun shares-page ()
  <settings-template>
    <taskie-page-title title= "Public Shared URLs" >
    </taskie-page-title>

       <p class= "mt-3 mb-2 text-muted" >Reports can be publicly shared from the report page. Use to share reports with stakeholders who are not on Screenshotbot.</p>

      <p class= "text-muted" >Keep track of all the shared URLs here, and revoke them if necessary.</p>



    ,(taskie-list
      :items (shares-for-company (current-company))
      :headers '("URL" "Shared by" "Expires" "Actions")
      :checkboxes nil
      :empty-message "No shares yet. Create a new share from any report."
      :row-generator (lambda (share)
                       (let* ((url (hex:make-full-url hunchentoot:*request*
                                                      'shared-report-page
                                                      :eoid (encrypt:encrypt-mongoid (oid-array share))))
                              (expiry-date (expiry-date share))
                              (expiry-ts (unless (str:emptyp expiry-date)
                                          (local-time:parse-timestring expiry-date))))
                         <taskie-row>
                         <span><a href=url >,(reverse (str:shorten 40 (reverse url))) </a></span>
                         <span>,(user-full-name (share-creator share))</span>
                         <span>,(cond
                                  ((share-revoked-p share)
                                 <span class= "text-warning">Revoked</span>)
                                ((null expiry-ts)
                                 "Never")
                                ((local-time:timestamp< expiry-ts
                                                        (local-time:now))
                                 <span>Expired ,(timeago :timestamp expiry-ts)</span>)
                                (t
                                 (timeago :timestamp expiry-ts))) </span>
                         <span>
                           ,(cond
                              ((and expiry-ts
                                    (local-time:timestamp< expiry-ts
                                                           (local-time:now)))
                               nil)
                              ((not (share-revoked-p share))
                               <a href= (nibble () (revoke-share share)) >Revoke</a>)
                              (t
                               <a href= (nibble () (undo-revoke-share share))>Undo Revoke</a>))
                         </span>
                         </taskie-row>
                         )))

  </settings-template>)

(defun revoke-share (share)
  (with-transaction ()
    (setf (share-revoked-p share) t))
  (hex:safe-redirect "/settings/shares"))

(defun undo-revoke-share (share)
  (with-transaction ()
    (setf (share-revoked-p share) nil))
  (hex:safe-redirect "/settings/shares"))
