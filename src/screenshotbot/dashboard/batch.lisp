;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/dashboard/batch
  (:use #:cl)
  (:import-from #:screenshotbot/server
                #:with-login
                #:defhandler)
  (:import-from #:util/store/object-id
                #:find-by-oid)
  (:import-from #:screenshotbot/user-api
                #:channel-name
                #:can-view!)
  (:import-from #:screenshotbot/template
                #:app-template)
  (:import-from #:screenshotbot/model/batch
                #:batch-item-channel
                #:batch-items)
  (:import-from #:screenshotbot/taskie
                #:taskie-row
                #:taskie-list)
  (:export
   #:batch-handler))
(in-package :screenshotbot/dashboard/batch)

(named-readtables:in-readtable markup:syntax)

(defhandler (batch-handler :uri "/batch/:oid") (oid)
  (with-login ()
    (let ((batch (find-by-oid oid)))
      (assert batch)
      (can-view! batch)
      (render-batch batch))))

(defun render-batch-item (item)
  (taskie-row
   :object item
   <span>,(channel-name (batch-item-channel item)) </span>))

(defmethod render-batch (batch)
  (let ((items (fset:convert 'list (batch-items batch))))
    <app-template>
      ,(taskie-list :empty-message "No runs in this batch yet"
                    :items items
                    :headers (list "Channel")
                    :row-generator #'render-batch-item)
    </app-template>))
