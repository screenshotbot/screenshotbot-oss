;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/slack/rules
  (:use #:cl)
  (:import-from #:bknr.datastore
                #:persistent-class
                #:store-object)
  (:import-from #:screenshotbot/model/recorder-run
                #:recorder-run-company
                #:recorder-run-tags
                #:recorder-run)
  (:import-from #:util/store/fset-index
                #:fset-set-index)
  (:import-from #:util/store/store
                #:defindex))
(in-package :screenshotbot/slack/rules)

(defindex +company-index+
  'fset-set-index
  :slot-name '%company)

(defclass tag-rule (store-object)
  ((%tag :initarg :tag
         :accessor tag-rule-tag)
   (%slack-channel :initarg :slack-channel
                   :accessor slack-channel)
   (%company :initarg :company
             :reader company
             :index +company-index+
             :index-reader tag-rules-for-company))
  (:metaclass persistent-class)
  (:documentation "If a run has a given tag, send a notification to the given channel"))

(defmethod auth:can-viewer-view (vc (self tag-rule))
  (auth:can-viewer-view vc (company self)))

(defmethod matches-rule ((self tag-rule) (run recorder-run))
  (assert (eql (company self)
               (recorder-run-company run)))
  (str:s-member
   (recorder-run-tags run)
   (tag-rule-tag self)))

(defmethod find-slack-channels-for-run ((run recorder-run))
  "Returns the list of slack channels. There might be duplicates if
multiple rules match."
  (loop for tag-rule in (fset:convert 'list
                                      (tag-rules-for-company
                                       (recorder-run-company run)))
        if (matches-rule tag-rule run)
          collect (slack-channel tag-rule)))


