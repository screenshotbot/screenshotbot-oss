;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(uiop:define-package :screenshotbot/github/webhook
  (:use #:cl #:alexandria)
  (:import-from #:bknr.datastore
                #:store-object
                #:persistent-class
                #:hash-index
                #:with-transaction)
  (:import-from #:screenshotbot/model/channel
                #:github-get-canonical-repo)
  (:import-from #:screenshotbot/server
                #:defhandler)
  (:import-from #:screenshotbot/model/company
                #:installation-id)
  (:import-from #:screenshotbot/model/recorder-run
                #:github-repo)
  (:import-from #:screenshotbot/github/plugin
                #:webhook-secret
                #:github-plugin)
  (:import-from #:util/threading
                #:with-extras
                #:make-thread
                #:max-pool
                #:ignore-and-log-errors)
  (:import-from #:util/request
                #:http-request)
  (:import-from #:util/store/store-migrations
                #:def-store-migration)
  (:export
   #:pull-request
   #:github-get-canonical-repo
   #:repo-full-name
   #:pull-request-id
   #:pull-request-head
   #:pull-request-base
   #:all-pull-requests
   #:pull-request-with-url))
(in-package :screenshotbot/github/webhook)

(defmethod github-get-canonical-repo (repo)
  (let ((host (if (str:containsp "bitbucket" repo)
                  "bitbucket.org"
                  "github.com")))
   (cl-ppcre:regex-replace-all
    (format nil "^(ssh://)?git@~a[:/]" host)
    (cl-ppcre:regex-replace-all
     "https://api."
     (cl-ppcre:regex-replace-all "[.]git$"
                                 (cl-ppcre:regex-replace-all "^git://"
                                  repo "https://")
                                 "")
     "https://")
    (format nil "https://~a/" host))))


(defhandler (nil :uri "/github-webhook") ()
  (error "No longer implemented"))


(def-store-migration ("Delete pull-request objects -- T1966" :version 35)
  (mapc #'bknr.datastore:delete-object
        (bknr.datastore:class-instances 'pull-request)))
