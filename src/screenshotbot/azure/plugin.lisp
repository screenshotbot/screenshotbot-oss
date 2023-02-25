;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/azure/plugin
  (:use #:cl)
  (:import-from #:screenshotbot/plugin
                #:plugin-parse-repo
                #:plugin)
  (:import-from #:bknr.datastore
                #:persistent-class)
  (:import-from #:bknr.datastore
                #:store-object)
  (:import-from #:bknr.indices
                #:unique-index)
  (:import-from #:util/store
                #:with-class-validation)
  (:import-from #:screenshotbot/installation
                #:find-plugin
                #:installation)
  (:import-from #:alexandria
                #:when-let)
  (:import-from #:screenshotbot/git-repo
                #:generic-git-repo)
  (:export
   #:azure-plugin
   #:azure-server
   #:azure-settings
   #:azure-access-token
   #:azure-settings-for-company
   #:azure-git-repo))
(in-package :screenshotbot/azure/plugin)

(defclass azure-plugin (plugin)
  ())

(with-class-validation
  (defclass azure-settings (store-object)
    ((%server :initarg :server
              :reader azure-server)
     (%personal-access-token
      :initarg :access-token
      :reader azure-access-token)
     (%company :reader company
               :index-type unique-index
               :index-reader azure-settings-for-company
               :initarg :company))
    (:metaclass persistent-class)))

(defclass azure-git-repo (generic-git-repo)
  ())

(defmethod plugin-parse-repo ((plugin azure-plugin)
                              company
                              repo-url)
  (when-let ((settings (azure-settings-for-company company)))
    (let ((hostname (quri:uri-host (quri:uri (azure-server settings)))))
      (when (str:containsp hostname repo-url)
        (make-instance 'azure-git-repo
                       :link repo-url)))))

(defun azure-plugin (&key (installation (installation)))
  (find-plugin installation 'azure-plugin))
