;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/insights/fuzz
  (:use #:cl)
  (:import-from #:screenshotbot/user-api
                #:singletonp
                #:company-name
                #:personalp
                #:adminp)
  (:import-from #:screenshotbot/server
                #:defhandler)
  (:import-from #:screenshotbot/template
                #:app-template)
  (:import-from #:screenshotbot/login/common
                #:with-login)
  (:import-from #:screenshotbot/model/company
                #:sub-companies-of)
  (:import-from #:screenshotbot/insights/dashboard
                #:render-analytics))
(in-package :screenshotbot/insights/fuzz)

(named-readtables:in-readtable markup:syntax)

(defclass fake-company ()
  ((sub-companies :initarg :sub-companies
                  :reader sub-companies-of)
   (screenshotbot/model/company::name :initform "Acme Corporation")))

(defmethod personalp ((Self fake-company))
  nil)

(defmethod singletonp ((self fake-company))
  nil)

(defmethod auth:can-view ((self fake-company) user)
  t)

;; use ids: 32046 to test on staging
(defhandler (nil :uri "/insights/fuzz") (ids)
  (with-login ()
    (assert (adminp (auth:current-user)))
    (let ((companies (mapcar #'bknr.datastore:store-object-with-id
                             (mapcar #'parse-integer (str:split "," ids)))))
      (render-analytics
       (make-instance 'fake-company :sub-companies companies)))))
