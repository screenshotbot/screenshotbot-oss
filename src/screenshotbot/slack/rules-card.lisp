;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/slack/rules-card
  (:use #:cl)
  (:import-from #:nibble
                #:nibble)
  (:import-from #:screenshotbot/slack/rules
                #:slack-channel
                #:tag-rule-tag
                #:tag-rule
                #:tag-rules-for-company)
  (:import-from #:core/ui/simple-card-page
                #:simple-card-page)
  
  
  )
(in-package :screenshotbot/slack/rules-card)

(named-readtables:in-readtable markup:syntax)

(markup:deftag rules-list ()
  <div class= "card mt-3">
    <div class= "card-header d-flex justify-content-between align-items-center">
      <h3>Notification Rules</h3>
      <a class= "btn btn-secondary" href= (nibble () (create-rule)) >Add Rule</a>
    </div>
    <div class= "card-body">
      ,(let ((rules (tag-rules-for-company (auth:current-company))))
         (cond
           ((fset:empty? rules)
            <div class= "d-flex justify-content-around text-muted">
              Click Add Rule above to create your first rule
            </div>)
           (t
            (render-rules-table rules))))
    </div>
  </div>)

(defun render-rules-table (rules)
  <table class= "table" >
    <tr>
      <td>Condition</td>
      <td>Slack channel</td>
    </tr>
    <tbody>
    ,@(loop for rule in (fset:convert 'list rules)
            collect
            <tr>
              <td>If the tag name is <tt>,(tag-rule-tag rule)</tt></td>
              <td><tt>,(str:ensure-prefix "#" (slack-channel rule)) </tt></td>
            </tr>)
    </tbody>
  </table>)

(defun create-rule ()
  <simple-card-page form-action= (nibble (tag slack-channel :method :post) (%post tag slack-channel)) >
    <div class= "card-header">
      <h3>Add Slack Notification Rule</h3>
    </div>
    <div>
      <div class= "mb-3">
        <label for= "tag" class= "form-label" >If the run has the following tag:</label>
        <input type= "text" name= "tag" id= "tag" class= "form-control" placeholder= "my-tag" />
      </div>

      <div class= "mb-3">
        <label for= "slack-channel" class= "form-label" >... then ping to the following Slack channel:</label>
        <input type= "text" name= "slack-channel" id= "slack-channel" class= "form-control" placeholder= "#my-team" />
      </div>
    </div>
    <div class= "card-footer d-flex align-items-center">
      <input type= "submit" class= "btn btn-primary" value= "Save Rule" />
      <a href= "#" class= "ms-3" >Cancel</a>
    </div>
  </simple-card-page>)

(defun %post (tag slack-channel)
  (make-instance 'tag-rule
                 :tag tag
                 :company (auth:current-company)
                 :slack-channel slack-channel)
  (hex:safe-redirect "/settings/slack"))
