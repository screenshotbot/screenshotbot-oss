;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/api/finalized-commit
  (:use #:cl)
  (:import-from #:screenshotbot/api/model
                #:decode-json)
  (:import-from #:screenshotbot/model/finalized-commit
                #:finalized-commit-hash
                #:finalized-commit)
  (:import-from #:screenshotbot/user-api
                #:current-company)
  (:import-from #:screenshotbot/api/core
                #:defapi)
  (:local-nicknames (#:dto #:screenshotbot/api/model)))
(in-package :screenshotbot/api/finalized-commit)

(defun %parse-body ()
  (let ((body (hunchentoot:raw-post-data :force-text t)))
    (decode-json body 'dto:finalized-commit)))

(defmethod to-dto ((self finalized-commit))
  (make-instance 'dto:finalized-commit
                 :commit (finalized-commit-hash self)))

(defapi (%post-finalized-commit :uri "/api/finalized-commit" :method :post
                                :use-yason t) ()
  (assert (current-company))
  (let ((input (%parse-body)))
    (to-dto
     (make-instance 'finalized-commit
                    :company (current-company)
                    :commit (dto:finalized-commit-hash input)))))
