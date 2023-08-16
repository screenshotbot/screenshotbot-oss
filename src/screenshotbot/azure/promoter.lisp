;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/azure/promoter
  (:use #:cl
        #:screenshotbot/azure/plugin)
  (:import-from #:screenshotbot/abstract-pr-promoter
                #:check-key
                #:make-promoter-for-acceptable
                #:details-url
                #:check-status
                #:check-title
                #:check
                #:push-remote-check
                #:abstract-pr-acceptable
                #:make-acceptable
                #:promoter-pull-id
                #:plugin-installed?
                #:valid-repo?
                #:abstract-pr-promoter)
  (:import-from #:screenshotbot/promote-api
                #:plugin-promoter)
  (:import-from #:screenshotbot/azure/plugin
                #:azure-access-token
                #:azure-plugin)
  (:import-from #:screenshotbot/model/company
                #:company)
  (:import-from #:screenshotbot/model/recorder-run
                #:recorder-run-company
                #:github-repo
                #:pull-request-id
                #:recorder-run)
  (:import-from #:bknr.datastore
                #:persistent-class)
  (:import-from #:util/store
                #:with-class-validation)
  (:import-from #:screenshotbot/azure/request
                #:git-status-context
                #:pull-request-status
                #:create-pull-request-status
                #:azure)
  (:import-from #:screenshotbot/user-api
                #:recorder-run-channel
                #:channel-name))
(in-package :screenshotbot/azure/promoter)

(defclass azure-promoter (abstract-pr-promoter)
  ())

(with-class-validation
  (defclass azure-acceptable (abstract-pr-acceptable)
    ()
    (:metaclass persistent-class)))

(defmethod plugin-promoter ((self azure-plugin))
  (make-instance 'azure-promoter))

(defmethod valid-repo? ((self azure-promoter) (repo azure-git-repo))
  t)

(defmethod plugin-installed? ((self azure-promoter)
                              (company company)
                              url)
  (azure-settings-for-company company))

(defmethod promoter-pull-id ((self azure-promoter)
                             (run recorder-run))
  (pull-request-id run))

(defmethod make-acceptable ((self azure-promoter)
                            report
                            &rest args)
  (apply #'make-instance 'azure-acceptable
         :report report
         args))

(defun parse-org-and-project (url hostname)
  (let* ((parts (str:split "/" url))
         (host-pos (loop for part in parts
                         for i from 0
                         if (str:containsp hostname part)
                           return i)))
    (values
     (elt parts (+ host-pos 1))
     (elt parts (+ host-pos 2))
     (car (last parts)))))

(defvar +succeeded+ "succeeded")
(defvar +failed+ "failed")

(defmethod push-remote-check ((self azure-promoter)
                              run
                              (check check))
  (let ((settings (azure-settings-for-company
                   (recorder-run-company run))))
    (multiple-value-bind (org project repo)
        (parse-org-and-project (github-repo run)
                               (azure-hostname settings))
      (let ((azure (make-instance 'azure
                                  :hostname (azure-hostname settings)
                                  :token (azure-access-token settings)
                                  :organization org
                                  :project project)))
        (create-pull-request-status
         azure
         (make-instance 'pull-request-status
                        :description (format nil "Screenshotbot: ~a"
                                             (check-title check))
                        :state (ecase (check-status check)
                                 (:accepted +succeeded+)
                                 (:rejected +failed+)
                                 (:success +succeeded+)
                                 (:failure +failed+)
                                 (:action-required +failed+))
                        :target-url (details-url check)
                        :context (make-instance 'git-status-context
                                                :name (check-key check)))
         :repository-id repo
         :pull-request-id (pull-request-id run))))))

(defmethod make-promoter-for-acceptable ((acceptable azure-acceptable))
  (make-instance 'azure-promoter))
