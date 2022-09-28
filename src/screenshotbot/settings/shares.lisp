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
    <div class= "card mt-3">
      <div class= "card-header">
        <h3>Public Shared URLs</h3>
        <p>Reports can be publicly shared from the report page. This is usually used to share reports with stakeholders who are not on Screenshotbot.</p>

        <p>Keep track of all the shared URLs here, and revoke them if necessary.</p>
      </div>

      <div class= "card-body">
        <table class= "table table-hover user-table align-items-center" >
          <thead>
            <tr>
              <th>URL</th>
              <th>Shared by</th>
              <th>Expires</th>
              <th>Actions</th>
            </tr>
          </thead>

          <tbody>
            ,@ (loop for share in (shares-for-company (current-company))
                     for url = (hex:make-full-url hunchentoot:*request*
                                                  'shared-report-page
                                                   :eoid (encrypt:encrypt-mongoid (oid-array share)))
                     for expiry-date = (expiry-date share)
                     for expiry-ts = (unless (str:emptyp expiry-date)
                                       (local-time:parse-timestring expiry-date))
                     collect

                     (util:copying (share)
                       <tr>
                         <td><a href=url >,(reverse (str:shorten 40 (reverse url))) </a></td>
                         <td>,(user-full-name (share-creator share))</td>
                         <td>,(cond
                                ((share-revoked-p share)
                                 <span class= "text-warning">Revoked</span>)
                                ((null expiry-ts)
                                 "Never")
                                ((local-time:timestamp< expiry-ts
                                                        (local-time:now))
                                 <span>Expired ,(timeago :timestamp expiry-ts)</span>)
                                (t
                                 (timeago :timestamp expiry-ts))) </td>
                         <td>
                           ,(cond
                              ((and expiry-ts
                                    (local-time:timestamp< expiry-ts
                                                           (local-time:now)))
                               nil)
                              ((not (share-revoked-p share))
                               <a href= (nibble () (revoke-share share)) >Revoke</a>)
                              (t
                               <a href= (nibble () (undo-revoke-share share))>Undo Revoke</a>))
                         </td>
                       </tr>))
          </tbody>

        </table>
      </div>
    </div>

  </settings-template>)

(defun revoke-share (share)
  (with-transaction ()
    (setf (share-revoked-p share) t))
  (hex:safe-redirect "/settings/shares"))

(defun undo-revoke-share (share)
  (with-transaction ()
    (setf (share-revoked-p share) nil))
  (hex:safe-redirect "/settings/shares"))
