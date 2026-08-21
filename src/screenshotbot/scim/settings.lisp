(defpackage #:screenshotbot/scim/settings
  (:use #:cl)
  (:import-from #:screenshotbot/settings-api
                #:defsettings
                #:settings-template)
  (:import-from #:screenshotbot/scim/model
                #:scim-configs-for-company
                #:scim-config-token
                #:scim-config)
  (:import-from #:nibble
                #:nibble)
  (:import-from #:core/ui/simple-card-page
                #:simple-card-page))
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
          ,@(loop for config in (fset:convert 'list (scim-configs-for-company (auth:current-company)))
                  collect
                  <div>
                    hello ,(last-four (scim-config-token config))
                  </div>)
          <form method="post" action= (nibble () (%create-token-flow))>
            <input type="submit" value="Create Token" class="btn btn-secondary"/>
          </form>
        </div>

        <table>

        </table>
        <div class="card-footer">
          <input type="submit" value="Save" class="btn btn-primary" />
        </div>
      </div>
    </settings-template>)))

(defun %create-token-flow ()
  (let ((config (make-instance 'scim-config :company (auth:current-company))))
    (hex:safe-redirect
     (nibble ()
       <simple-card-page>
         <div class="mb-1">
           <label for="token" class="form-label">
             Your new SCIM token
           </label>
           <input type="disabled" value=(scim-config-token config) class="form-control" />
         </div>
         <div class="card-footer">
           <a href="/settings/scim" class="btn btn-primary">Done</a>
         </div>
       </simple-card-page>))))

(defun last-four (str)
  (format nil "********~a"
          (str:substring (- (length str) 4) nil str)))
