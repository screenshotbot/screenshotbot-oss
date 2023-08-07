;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/api/unchanged-run
  (:use #:cl)
  (:import-from #:screenshotbot/api/core
                #:defapi)
  (:import-from #:screenshotbot/model/recorder-run
                #:unchanged-run-other-commit
                #:unchanged-run-commit
                #:unchanged-run-channel
                #:unchanged-run)
  (:import-from #:screenshotbot/user-api
                #:channel-name
                #:current-company)
  (:import-from #:screenshotbot/model/company
                #:find-or-create-channel)
  (:local-nicknames (#:dto #:screenshotbot/api/model)))
(in-package :screenshotbot/api/unchanged-run)

(defun parse-body (class-name)
  (let ((body (hunchentoot:raw-post-data :force-text t)))
    (json-mop:json-to-clos body class-name)))

(defun to-dto (ret)
  (make-instance 'dto:unchanged-run
                 :id (bknr.datastore:store-object-id ret)
                 :channel (channel-name (unchanged-run-channel ret))
                 :commit (unchanged-run-commit ret)
                 :other-commit (unchanged-run-other-commit ret)))

(defapi (%post-unchanged-run :uri "/api/unchanged-run" :method :post
         :use-yason t) ()
  (assert (current-company))
  (let ((input (parse-body 'dto:unchanged-run)))
    (to-dto
     (make-instance 'unchanged-run
                    :channel (find-or-create-channel
                              (current-company)
                              (dto:unchanged-run-channel input))
                    :commit (dto:unchanged-run-commit input)
                    :other-commit (dto:unchanged-run-other-commit input)))))
