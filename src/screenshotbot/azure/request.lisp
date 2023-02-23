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

(defclass build-definition-reference ()
  ((name :json-key "name"
         :json-type :string
         :reader name)
   (id :json-key "id"
       :json-type :number
       :reader id))
  (:metaclass ext-json-serializable-class))


(defvar *token* nil)

(defclass azure ()
  ((token :initarg :token
          :reader token)
   (organization :initarg :organization
                 :reader organization)
   (project :initarg :project
            :reader project)))

(defvar *test-azure* (make-instance 'azure :token *token*
                                           :organization "testsbot"
                                           :project "fast-example"))

(defun azure-request (azure url &key
                                  method
                                  response-type
                                  content)
  (multiple-value-bind (response code)
      (http-request
       (format nil "https://dev.azure.com/~a/~a/_apis/~a?api-version=7.0"
               (organization azure)
               (project azure)
               url)
       :content (with-output-to-string (out)
                  (yason:encode content out))
       :content-type "application/json"
       :basic-authorization (list "" (token azure))
       :accept "application/json"
       :method method
       :want-string t)
    (cond
      ((<= 400 code 510)
       (format t "Got failure: ~a" response))
      (t
       (json-mop:json-to-clos response response-type)))))

(defun queue-build (azure &key
                      uri
                      result)
  (let ((body (make-instance 'devops-build
                             :uri "https://screenshotbot.io/runs"
                             :build-result "succeeded")))
    (azure-request
     azure
     "build/builds"
     :method :post
     :content body)))

(defclass list-definitions-response ()
  ((value :json-type (:list build-definition-reference)
          :json-key "value"
          :reader build-definitions))
  (:metaclass ext-json-serializable-class))

(defun list-definitions (azure)
  (azure-request
   azure
   "build/definitions"
   :method :get
   :response-type 'list-definitions-response))


#+nil
(format t "~a"(list-definitions *test-azure*))
