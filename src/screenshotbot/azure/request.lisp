;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/azure/request
  (:use #:cl)
  (:import-from #:util/json-mop
                #:ext-json-serializable-class)
  (:import-from #:util/request
                #:http-request))
(in-package :screenshotbot/azure/request)

#|
Documentation
https://learn.microsoft.com/en-us/rest/api/azure/devops/build/status/get?view=azure-devops-rest-7.0
|#


(defclass devops-build ()
  ((uri :initarg :uri
        :json-key "uri"
        :json-type :string)
   (build-result :initarg :build-result
                 :json-key "result"
                 :json-type :string))
  (:metaclass ext-json-serializable-class))

(defvar *token*)

(defun azure-request (url &key (token *token*) organization project
                            method
                            content)
  (multiple-value-bind (response code)
      (http-request
       (format nil "https://dev.azure.com/~a/~a/_apis/~a?api-version=7.0"
               organization
               project
               url)
       :content (with-output-to-string (out)
                  (yason:encode content out))
       :content-type "application/json"
       :basic-authorization (list "" token)
       :accept "application/json"
       :method method
       :want-string t)
    (cond
      ((<= 400 code 510)
       (format t "Got failure: ~a" response))
      (t
       response))))

(defun queue-build (&key
                      token
                      organization
                      project
                      uri
                      result)
  (let ((body (make-instance 'devops-build
                             :uri "https://screenshotbot.io/runs"
                             :build-result "succeeded")))
    (azure-request
     "build/builds"
     :token token
     :organization organization
     :project project
     :method :post
     :content body)))

(defun list-definitions (&key token
                           organization
                           project)
  (azure-request
   "build/definitions"
   :token token
   :organization organization
   :project project
   :method :get))


#+nil
(format t "~a"(list-definitions :token *token*
                                :organization "testsbot"
                                :project "fast-example"))
