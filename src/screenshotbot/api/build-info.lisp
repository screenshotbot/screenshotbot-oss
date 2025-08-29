;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/api/build-info
  (:use #:cl)
  (:import-from #:screenshotbot/model/build-info
                #:find-or-create-build-info
                #:find-build-info
                #:build-info-repo-url
                #:build-url
                #:build-info)
  (:import-from #:screenshotbot/api/core
                #:defapi)
  (:local-nicknames (#:dto #:screenshotbot/api/model)))
(in-package :screenshotbot/api/build-info)

(defmethod to-dto ((self build-info))
  (make-instance 'dto:build-info
                 :build-url (build-url self)
                 :repo-url (build-info-repo-url self)))

(defun %parse-build-info-body ()
  (let ((body (hunchentoot:raw-post-data :force-text t)))
    (dto:decode-json body 'dto:build-info)))

(defapi (%get-build-info :uri "/api/build-info" :method :get
                         :use-yason t) (build-url)
  (let ((build-info (find-build-info
                     (auth:current-company)
                     build-url)))
    (when build-info
      (to-dto build-info))))

(defapi (%post-build-info :uri "/api/build-info" :method :put
                          :use-yason t) ()
  (let ((input (%parse-build-info-body)))
    (let ((build-info (find-or-create-build-info
                       (auth:current-company)
                       (dto:build-info-build-url input))))
      (setf (build-info-repo-url build-info)
            (dto:build-info-repo-url input))
      (to-dto build-info))))



