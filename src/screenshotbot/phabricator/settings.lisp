;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(uiop:define-package :screenshotbot/phabricator/settings
  (:use #:cl
        #:alexandria
        #:markup
        #:nibble
        #:util/form-errors
        #:screenshotbot/model/company
        #:screenshotbot/user-api
        #:screenshotbot/settings-api)
  (:import-from #:screenshotbot/phabricator/plugin
                #:phabricator-plugin)
  (:import-from #:bknr.datastore
                #:with-transaction))
(in-package :screenshotbot/phabricator/settings)

(markup:enable-reader)

(deftag phabricator-settings-template ()
  (let* ((config (phabricator-config-for-company (current-company)))
         (submit (nibble (url api-key)
                   (let ((errors))
                     (flet ((check (field test message)
                              (unless test
                                (push (cons field message)
                                      errors))))
                       (check :url (not (str:emptyp url))
                              "Please provide a URL for phabricator")
                       (check :api-key (not (str:emptyp api-key))
                              "Please provide an API Key for Conduit")
                       (cond
                         (errors
                          (with-form-errors (:errors errors :url url :api-key api-key
                                             :was-validated t)
                            (phabricator-settings-template)))
                         (t
                          (with-transaction ()
                            (setf (phabricator-url config) url)
                            (setf (conduit-api-key config) api-key))
                          (phabricator-settings-template))))))))
    <settings-template>
      <form action= submit method= "POST" >
        <div class= "card mt-3">
          <div class= "card-header">
            <h3>Setup Phabricator Comments</h3>
          </div>

          <div class= "card-body">

            <div class= "mb-3">
              <label for= "url" class= "form-label">Phabricator Url</label>
              <input type= "text" class= "form-control" name= "url" placeholder= "https://example.phabricator.com" value= (phabricator-url config) />
            </div>

            <div class= "mb-3">
              <label for= "api-key" class= "form-label">Conduit API Key</label>
              <input type= "password" class= "form-control" name= "api-key" placeholder= "*****" value= (conduit-api-key config) />
            </div>

          </div>

          <div class= "card-footer">
            <input type= "submit" class= "btn btn-primary" value= "Done" />
          </div>
        </div>
      </form>
    </settings-template>))

(defsettings phabricator-settings
  :name "phabricator"
  :title "Phabricator"
  :section :vcs
  :plugin 'phabricator-plugin
  :handler 'phabricator-settings-template)
