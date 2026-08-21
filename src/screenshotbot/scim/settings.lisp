(defpackage #:screenshotbot/scim/settings
  (:use #:cl)
  (:import-from #:screenshotbot/settings-api
                #:defsettings
                #:settings-template))
(in-package #:screenshotbot/scim/settings)

(named-readtables:in-readtable markup:syntax)

(defsettings scim-settings
             :name "scim"
             :section :organization
             :title "SCIM"
             :handler (lambda ()
                        (scim-settings-page)))

(defun scim-settings-page ()
  (cond
   ((not (gk:check :scim-allowed (auth:current-company)))
    <settings-template>
      <div class="alert alert-warning">
        <strong>SCIM is currently disabled.</strong>
        Please <a href="/ticket/create">contact us</a> to enable SCIM for this organization
      </div>
    </settings-template>)
   ((not (roles:has-role-p (auth:current-company)
                           (auth:current-user)
                           'roles:owner))
    <settings-template>
      <div class="alert alert-info">
        You must be an organization Owner to update the SCIM configuration

      </div>
    </settings-template>)
   (t
    <settings-template>
      <div class="card">
        <div class="card-header">
          <h3>SCIM</h3>
        </div>

        <div class="card-body">
          <h4>Bearer tokens</h4>
          <!-- todo: I still don't know what this looks like -->
        </div>

        <table>

        </table>
        <div class="card-footer">
          <input type="submit" value="Save" class="btn btn-primary" />
        </div>
      </div>
    </settings-template>)))
