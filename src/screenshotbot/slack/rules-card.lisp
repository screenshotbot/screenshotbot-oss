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
                #:confirmation-page
                #:simple-card-page)
  (:import-from #:core/ui/mdi
                #:mdi)
  (:import-from #:util/copying
                #:copying)
  (:import-from #:bknr.datastore
                #:delete-object)
  (:import-from #:util/form-errors
                #:with-error-builder))
(in-package :screenshotbot/slack/rules-card)

(named-readtables:in-readtable markup:syntax)

(markup:deftag rules-list (&key test-slack-channel-callback)
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
            (render-rules-table rules :test-slack-channel-callback test-slack-channel-callback))))
    </div>
  </div>)

(markup:deftag delete-button (&key action)
  <form class= "d-inline" >
    <button type= "submit" class="btn btn-outline-danger" value= "Delete" formaction=action >
      <mdi name= "delete" /> Delete
    </button>
  </form>)

(markup:deftag test-button (&key action)
  <form class= "d-inline" >
    <button type= "submit" class="btn btn-outline-secondary" value= "Test" formaction=action >
      <mdi name= "play_arrow" /> Test
    </button>
  </form>)


(defun render-rules-table (rules &key test-slack-channel-callback)
  <table class= "table" >
    <thead>
      <tr>
        <th>Condition</th>
        <th>Slack channel</th>
        <th>Actions</th>
      </tr>
    </thead>
    <tbody>
    ,@(loop for rule in (fset:convert 'list rules)
            collect
            (copying (rule)
              <tr>
                <td class= "align-middle" >If the tag name is <tt>,(tag-rule-tag rule)</tt></td>
                <td class= "align-middle" ><tt>,(str:ensure-prefix "#" (slack-channel rule)) </tt></td>
                <td class= "align-middle" >
                  <test-button action= (nibble () (funcall test-slack-channel-callback (slack-channel rule))) />
                  <delete-button action= (nibble () (%delete rule)) />
                </td>
              </tr>))
    </tbody>
  </table>)

(defun go-home ()
  (hex:safe-redirect "/settings/slack"))

(defun %delete (rule)
  <confirmation-page yes= (nibble () (delete-object rule) (go-home))
                     no= (nibble () (go-home))>
    Are you sure you want to delete the rule for <tt>,(str:ensure-prefix "#" (slack-channel rule))</tt>?
  </confirmation-page>)

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
  (with-error-builder (:check check
                       :errors errors
                       :form-builder (create-rule)
                       :form-args (:tag tag
                                   :slack-channel slack-channel)
                       :success (progn
                                  (make-instance 'tag-rule
                                                 :tag tag
                                                 :company (auth:current-company)
                                                 :slack-channel slack-channel)
                                  (go-home)))
    (check :tag (str:non-blank-string-p tag)
           "Must provide a tag name")
    (check :slack-channel (str:non-blank-string-p slack-channel)
           "Must provide a slack-channel name")
    (check :slack-channel (not (str:containsp "," slack-channel))
           "Only provide one slack-channel")))
