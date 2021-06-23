;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(pkg:define-package :screenshotbot/dashboard/api-keys
    (:use #:cl
          #:alexandria
          #:nibble
          #:../user-api
          #:../api-key-api)
  (:import-from #:../server
                #:defhandler)
  (:import-from #:../template
                #:dashboard-template)
  (:import-from #:../ui
                #:confirmation-page
                #:simple-card-page)
  (:import-from #:../server
                #:with-login))

(markup:enable-reader)


(defun %create-api-key (user company)
  (let ((api-key (make-instance 'api-key
                                 :user user
                                 :company company)))
    <simple-card-page>
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
  (let* ((api-keys (user-api-keys user company))
         (create-api-key (nibble ()
                           (%create-api-key user company))))
    <dashboard-template user=user company=company script-name= script-name >
      <h1>API Keys</h1>

      <form method= "post" >
        <input type= "submit" formaction=create-api-key formmethod= "post"
               class= "btn btn-success" value= "New API Key" />
      </form>
      ,@ (loop for api-key in api-keys
               collect
               (let* ((api-key api-key)
                      (coded-secret (format nil
                                            "~a~a"
                                            (str:repeat 36 "*")
                                            (str:substring  36 nil (api-key-secret-key api-key))))
                      (delete-api-key (nibble ()
                                        (%confirm-delete api-key))))
                 <div>
                   <table>
                     <tr>
                       <td><b>API Key</b></td>
                       <td>,(api-key-key api-key)
                       <form style="display:inline-block" class= "ml-4" method= "post" >
                         <input type= "submit" formaction=delete-api-key
                                formmethod= "post"
                                class= "btn btn-danger"
                                value= "Delete" />
                       </form>
                       </td>
                     </tr>
                     <td class= "pr-3" ><b>API Secret</b></td>
                     <td>,(progn coded-secret)</td>

                   </table>
                   <p>To use this from your desktop, create a file <tt>$HOME/.screenshotbot</tt> with the following contents</p>

                   <pre>,(json:encode-json-to-string (list (cons :api-key (api-key-key api-key))
                                                           (cons :api-secret-key  coded-secret)))

                   </pre>

                 </div>))

    </dashboard-template>))

(defhandler (api-keys :uri "/api-keys") ()
  (with-login ()
   (%api-key-page)))
