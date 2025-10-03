;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/microsoft-teams/model
  (:use #:cl)
  (:import-from #:util/store/store
                #:defindex
                #:with-class-validation)
  (:import-from #:bknr.datastore
                #:persistent-class
                #:store-object)
  (:import-from #:util/store/fset-index
                #:fset-set-index)
  (:export
   #:teams-workflows-for-channel
   #:workflow-name
   #:webhook-url))
(in-package :screenshotbot/microsoft-teams/model)


(defindex +channel-index+
  'fset-set-index
  :slot-name '%channel)

(with-class-validation
  (defclass teams-workflow (store-object)
    ((%name :initarg :name
            :reader workflow-name)
     (%webhook-url :initarg :webhook-url
                   :reader webhook-url)
     (%channel :initarg :channel
               :index +channel-index+
               :index-reader teams-workflows-for-channel))
    (:metaclass persistent-class)))
