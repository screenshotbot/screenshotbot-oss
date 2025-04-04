;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(uiop:define-package :screenshotbot/api/core
  (:use #:cl #:alexandria)
  (:import-from #:screenshotbot/server
                #:defhandler)
  (:import-from #:screenshotbot/model/api-key
                #:mark-api-key-used
                #:%find-api-key
                #:api-key-secret-key)
  (:import-from #:screenshotbot/user-api
                #:api-key-company
                #:api-key-user
                #:current-user)
  (:import-from #:easy-macros
                #:def-easy-macro)
  (:import-from #:hunchentoot-extensions
                #:make-name)
  (:import-from #:util/json-mop
                #:json-mop-to-string
                #:ext-json-serializable-class)
  (:import-from #:screenshotbot/events
                #:with-tracing)
  (:import-from #:util/events
                #:push-event)
  (:import-from #:screenshotbot/model/company
                #:redirect-url
                #:maybe-redirect-for-company)
  (:import-from #:auth/viewer-context
                #:api-viewer-context)
  (:import-from #:util/threading
                #:with-extras)
  (:export
   #:defapi
   #:result
   #:error-result
   #:api-error
   #:id
   #:type
   #:api-result
   #:api-response
   #:*dtd*
   #:*api-key*))
(in-package :screenshotbot/api/core)

(defparameter *dtd*
  (asdf:system-relative-pathname :screenshotbot "dtd/api.dtd"))

(defvar *api-key* nil
  "The current API key being used")

(defvar *wrap-internal-errors* t
  "Wrap internal errors in a way that can be presented better to the
user. The intention of this flag is to set to NIL for some tests.")

(def-easy-macro with-api-key (&binding api-key &binding api-secret &fn fn)
  (multiple-value-bind (key secret) (hunchentoot:authorization)
    (cond
      (key
       (funcall fn key secret))
      (t
       (funcall fn (hunchentoot:parameter "api-key")
                (hunchentoot:parameter "api-secret-key"))))))

(defmethod authenticate-api-request ((request auth:authenticated-request))
  "Like auth:authenticate-request, but for API handling, and modifies the request"
  (with-api-key (api-key api-secret-key)
    (let ((key (%find-api-key  api-key)))
      (unless key
        ;; Remove some noise from sdk-integration-tests
        (unless (equal "deliver-sdk" api-key)
          (warn "No such API key: ~a" api-key))
        (error 'api-error
               :message (format nil "No such API key: ~a" api-key)))
      (unless (or
               (not key) ;; setings api
               (equal api-secret-key (api-key-secret-key key)))
        (error 'api-error
               :message "API secret key doesn't match what we have on record"))
      (mark-api-key-used key)
      (prog1
          (authenticate-request-from-key request key)

        ;; TODO: this probably never happens
        (unless (current-user)
          (error 'api-error
                 :message (format nil "API key appears to be invalid or non-existant, got: ~a" api-key)))))))

(defmethod authenticate-request-from-key ((request auth:authenticated-request) key)
  (let ((user (api-key-user key)))
    (setf
     (auth:request-user hunchentoot:*request*) user)
    (setf
     (auth:viewer-context hunchentoot:*request*)
     (make-instance 'api-viewer-context
                    :user user
                    :api-key key)))
  (let ((company (api-key-company key)))
    (when (redirect-url company)
      (hex:forward-request (redirect-url company)))
    ;; Do not check this! Old keys will still be valid against the
    ;; company!
    ;;(auth:can-view! company)
    (setf
     (auth:request-account hunchentoot:*request*) company))

  key)

(defun %funcall-with-api-handling (fn)
  (log:trace "Got parameters: ~s" (hunchentoot:post-parameters hunchentoot:*request*))
  (push-event :api-client-version
              :version (hunchentoot:header-in* :x-client-version))
  (let ((*api-key* (authenticate-api-request hunchentoot:*request*)))
    (funcall fn)))

(defun %set-error-code (e)
  ;; For api-version < 14, or SDK version <= 2.8.14, we would always
  ;; return error code 200.
  (setf (hunchentoot:return-code*) (api-error-code e))
  (setf (hunchentoot:content-type*) "application/json"))

(def-easy-macro with-error-handling (&key wrap-success &fn fn)
  "Converts internal errors into a JSON renderable object.

It should be safe to mock call-with-error-handling to just call
function fn for the purpose of tests."
  (let ((stacktrace-id (random 1000000000)))
    (with-extras (("stacktrace-id" stacktrace-id)
                  ("x-client-version" (hunchentoot:header-in* :x-client-version))
                  ("x-client-api-version" (hunchentoot:header-in* :x-client-api-version)))
     (block error-handling
       (flet ((%trace ()
                (format nil "Stacktrace ID: ~a. Please share this ID if contacting support." stacktrace-id)))
         (handler-bind ((api-error (lambda (e)
                                     (when *wrap-internal-errors*
                                       (%set-error-code e)
                                       (log:warn "API error: ~a" (api-error-msg e))
                                       (return-from error-handling
                                         (make-instance 'error-result
                                                        :success nil
                                                        :stacktrace (%trace)
                                                        :error (princ-to-string e))))))
                        (error  (lambda (e)
                                  (when *wrap-internal-errors*
                                    (%set-error-code e)
                                    (log:warn "Error: ~a" e)
                                    (sentry-client:capture-exception e)
                                    (return-from error-handling
                                      (make-instance 'error-result
                                                     :success nil
                                                     :stacktrace (%trace)
                                                     :error (format nil
                                                                    "Internal error, please contact support@screenshotbot.io: ~a"
                                                                    (princ-to-string e)))))
                                  )))
           (cond
             (wrap-success
              (make-instance 'result
                             :success t
                             :response
                             (fn)))
             (t
              (fn)))))))))

(defmacro defapi ((name &key uri method intern
                          (type :v1)
                          (use-yason nil)
                          (wrap-success t)
                          (listp nil))
                  params &body body)
  (declare (ignore type))
  (let* ((param-names (loop for param in params
                            if (symbolp param)
                              collect param
                            else
                              collect (car param)))
         (name (or name (make-name uri method)))
         (handler-name
           (when name (intern (format nil "~a-API-HANDLER" name) (symbol-package name)) )))
    (multiple-value-bind (body decls doc) (uiop:parse-body body)
      `(progn
         (defun ,name (&key ,@param-names)
           ,doc
           ,@decls
           (with-tracing (,uri)
             ,@body))
         (defhandler (,handler-name :uri ,uri :method ,method :intern ,intern) ,params
           ,@decls
           (flet ((ret ()
                    (%funcall-with-api-handling
                     (lambda ()
                      (,name
                       ,@ (loop for name in param-names
                                appending (list (intern (string name) "KEYWORD") name)))))))
             (setf (hunchentoot:header-out :content-type) "application/json; charset=utf-8")
             (,(cond
                 (use-yason
                  'json-mop-to-string)
                 (t
                  'json:encode-json-to-string))
              (with-error-handling (:wrap-success ,wrap-success)
                (let ((obj (ret)))
                  (or
                   obj
                   (when ,listp
                     #())))))))))))

(defun write-xml-output (ret)
  (setf (hunchentoot:header-out :content-type) "applicaton/xml")
  (with-output-to-string (out)
    (json-mop:encode ret out)))

(defclass result ()
  ((success :type boolean
            :initform t
            :json-key "success"
            :json-type :bool
            :initarg :success)
   (response :initarg :response
             :json-key "response"
             :json-type (or null :any)))
  (:metaclass ext-json-serializable-class))

(defclass error-result (result)
  ((error :initarg :error
          :reader error-result-message
          :json-key "error"
          :json-type :string)
   (stacktrace :initarg :stacktrace
               :reader error-result-stacktrace
               :json-key "stacktrace"
               :json-type (or null :string)))
  (:metaclass ext-json-serializable-class))

(define-condition api-error (error)
  ((message :initarg :message
            :reader api-error-msg)
   (code :initarg :code
         :initform 400
         :reader api-error-code))
  (:report (lambda (e out)
             (format out "~a" (api-error-msg e)))))

(defun api-error (fmt &rest args)
  (error 'api-error
         :message (apply #'format nil fmt args)))

(defmethod api-error-code (error)
  500)

(defapi (nil :uri "/api/test") ()
  3)

(defapi (nil :uri "/api/test-error") ()
  (error 'api-error :message "Foo"))

(defapi (nil :uri "/api/test-internal-error") ()
  (error "bad stuff"))

(defclass api-result () ())

(defclass api-response () ())
