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
                #:http-request)
  (:import-from #:screenshotbot/audit-log
                #:with-audit-log)
  (:import-from #:screenshotbot/azure/audit-log
                #:pr-update-request)
  (:import-from #:alexandria
                #:assoc-value))
(in-package :screenshotbot/azure/request)


(defclass git-status-context ()
  ((genre :initform "screenshotbot"
          :json-key "genre"
          :json-type :string)
   (name :initarg :name
         :json-key "name"
         :json-type :string))
  (:metaclass ext-json-serializable-class))

(defclass pull-request-status ()
  ((description :json-key "description"
                :json-type :string
                :initarg :description)
   (state :json-key "state"
          :json-type :string
          :initarg :state)
   (context :initarg :context
            :json-key "context"
            :json-type git-status-context)
   (target-url :initarg :target-url
               :json-key "targetUrl"
               :json-type :string))
  (:metaclass ext-json-serializable-class))


(defvar *token* nil)

(defclass azure ()
  ((token :initarg :token
          :reader token)
   (hostname :initarg :hostname
             :initform "dev.azure.com"
             :reader hostname)
   (organization :initarg :organization
                 :reader organization)
   (project :initarg :project
            :reader project)))

(defvar *test-azure* (make-instance 'azure :token *token*
                                           :organization "testsbot"
                                           :project "fast-example"))

(define-condition azure-error (simple-error)
  ((headers :initarg :headers)))

(defun azure-request (azure url &key
                                  method
                                  response-type
                                  parameters
                                  content)
  (declare (optimize (speed 0) (debug 3)))
  (let ((content (with-output-to-string (out)
                   (yason:encode content out))))
    (log:info "Sending body: ~a" content)
    (multiple-value-bind (response code headers)
       (http-request
        (format nil "https://~a/~a/~a/_apis/~a?api-version=7.0"
                (hostname azure)
                (organization azure)
                (project azure)
                url)
        :content content
        :content-type "application/json"
        :basic-authorization (list "" (token azure))
        :accept "application/json"
        :parameters parameters
        :method method
        :want-string t)
     (cond
       ((<= 400 code 510)
        (error 'azure-error
               :headers headers
               :format-control "Got failure: ~a"
               :format-arguments (list response)))
       ((str:containsp "text/html" (assoc-value headers :content-type))
        (error 'azure-error
               :headers headers
               :format-control "API returned HTML, not JSON. This is likely because the Personal Access Token is incorrect."))
       (t
        (json-mop:json-to-clos response response-type))))))

(defmethod create-pull-request-status (azure status
                                       &key repository-id
                                         company
                                         pull-request-id)
  (declare (optimize (speed 0) (debug 3)))
  (with-audit-log (audit-log (make-instance 'pr-update-request
                                            :pr-id pull-request-id
                                            :company company
                                            :repository-id repository-id))
    (declare (ignore audit-log))
    (azure-request
     azure
     (format nil
             "git/repositories/~a/pullRequests/~a/statuses"
             repository-id
             pull-request-id)
     :method :post
     :response-type 'pull-request-status
     :content status)))

#|
https://learn.microsoft.com/en-us/azure/devops/repos/git/branch-policies?view=azure-devops&tabs=browser#build-validation
|#

#+nil
(create-pull-request-status
 *test-azure*
 (make-instance 'pull-request-status
                :description "some stuff"
                :state "failed"
                :target-url "https://screenshotbot.io/runs"
                :context (make-instance 'git-status-context
                                        :name "foobar"))
 :repository-id "fast-example"
 :pull-request-id 1)
