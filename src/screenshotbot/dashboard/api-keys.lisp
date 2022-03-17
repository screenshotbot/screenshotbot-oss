;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(uiop:define-package :screenshotbot/dashboard/api-keys
  (:use #:cl
        #:alexandria
        #:nibble
        #:screenshotbot/user-api
        #:screenshotbot/api-key-api)
  (:import-from #:screenshotbot/server
                #:defhandler)
  (:import-from #:screenshotbot/template
                #:dashboard-template)
  (:import-from #:screenshotbot/ui
                #:confirmation-page
                #:simple-card-page)
  (:import-from #:screenshotbot/server
                #:with-login))
(in-package :screenshotbot/dashboard/api-keys)

(markup:enable-reader)


(defun %create-api-key (user company)
  (let ((api-key (make-instance 'api-key
                                 :user user
                                 :company company)))
    <simple-card-page max-width= "80em" >
      <div class= "card-header">
        <h3>New API Key</h3>
      </div>
      <p>Please copy paste the API secret, you won't have access to it again. But you will be able to create new API keys as needed.</p>

      <div>
        <b>API Key</b>: ,(api-key-key api-key)<br />
        <b>API Secret</b>: ,(api-key-secret-key api-key)
      </div>

      <div class= "card-footer">
        <a href= "/api-keys">Go back</a>
      </div>

    </simple-card-page>))

(defun %confirm-delete (api-key)
  (confirmation-page
   :yes (nibble ()
          (delete-api-key api-key)
          (hex:safe-redirect "/api-keys"))
   :no (nibble ()
         (hex:safe-redirect "/api-keys"))
   <p>Are you sure you want to delete API Key ,(api-key-key api-key)</p>))

(defun %api-key-page (&key (user (current-user))
                        (company (current-company))
                        (script-name "/api-keys"))
  (declare (ignore script-name))
  (let* ((api-keys (user-api-keys user company))
         (create-api-key (nibble ()
                           (%create-api-key user company))))
    <simple-card-page max-width= (if api-keys "60rem" "30rem") >
      <div class= "card-header">
        <div class= "d-flex flex-row justify-content-between">
          <div class= "d-flex ">
            <h3>API Keys</h3>
          </div>
          <div class= "d-flex align-items-center">
            <form method= "post">
              <input type= "submit" formaction=create-api-key formmethod= "post"
                     class= "btn btn-success" value= "New API Key" />
            </form>
          </div>
        </div>



      </div>
      ,(cond
         (api-keys
          (api-keys-table api-keys))
         (t
          <div class= "text-center mt-3 mb-3">

            <p>
              You don't have any API keys.
            </p>

            <p>
              You will use an API key to upload screenshots with the <a href= "https://docs.screenshotbot.io/docs/recorder-cli/" target= "_blank">Screenshotbot SDK</a>. You don't need an API key for web projects.
            </p>
          </div>))
    </simple-card-page>))

(defun api-keys-table (api-keys)
        <table class= "table table-hover">
        <thead>
          <tr>
            <th>API Key</th>
            <th>API Secret</th>
            <th>Actions</th>
          </tr>
        </thead>
        <tbody>
      ,@ (loop for api-key in api-keys
               collect
               (let* ((api-key api-key)
                      (coded-secret (format nil
                                            "~a~a"
                                            (str:repeat 36 "*")
                                            (str:substring  36 nil (api-key-secret-key api-key))))
                      (delete-api-key (nibble ()
                                        (%confirm-delete api-key))))
                 <tr class= "align-middle" >
                   <td class= "" >,(api-key-key api-key)</td>
                   <td>,(progn coded-secret)</td>

                   <td>
                     <form style="display:inline-block" class= "ml-4 float-end" method= "post" >
                       <input type= "submit" formaction=delete-api-key
                              formmethod= "post"
                              class= "btn btn-danger"
                              value= "Delete" />
                     </form>
                   </td>
                 </tr>))

        </tbody>

      </table>)

(defhandler (api-keys :uri "/api-keys") ()
  (with-login ()
   (%api-key-page)))
