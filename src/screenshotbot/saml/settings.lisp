;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/saml/settings
  (:use #:cl)
  (:import-from #:screenshotbot/server
                #:defhandler)
  (:import-from #:screenshotbot/template
                #:app-template)
  (:import-from #:core/ui/taskie
                #:taskie-list)
  (:import-from #:screenshotbot/login/saml
                #:saml-entity-id
                #:saml-auth-providers-for-company)
  (:import-from #:screenshotbot/login/common
                #:with-login))
(in-package :screenshotbot/saml/settings)

(named-readtables:in-readtable markup:syntax)

(defun check-enabled! ()
  (assert (gk:check :self-service-saml (auth:current-company))))

(defhandler (nil :uri "/sso") ()
  "List all the SAML configs"
  (check-enabled!)
  (with-login ()
    <app-template>
      <div class= "page-title-box">
        <h4>Self service SSO configs</h4>
      </div>

      <table class= "table border">
        <thead>
          <th>Type</th>
          <th>IdP Entity ID</th>
          <th>Actions</th>
        </thead>
        ,@(loop for saml in (fset:convert 'list (saml-auth-providers-for-company (auth:current-company)))
                collect
                <tr class= "align-middle" >
                  <td>SAML</td>
                  <td><tt>,(saml-entity-id saml)</tt></td>
                  <td>
                    <a href= "#" class= "btn btn-sm btn-secondary" >Edit</a>
                    <a href= "#" class= "btn btn-sm btn-danger" >Delete</a>
                  </td>
                </tr>)
      </table>
    </app-template>))


