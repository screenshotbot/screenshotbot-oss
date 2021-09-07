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
                #:%find-api-key
                #:api-key-secret-key)
  (:import-from #:screenshotbot/user-api
                #:*current-api-key*
                #:current-user)
  (:export
   #:defapi
   #:result
   #:error-result
   #:api-error
   #:id
   #:type
   #:api-result
   #:api-response))
(in-package :screenshotbot/api/core)

(defun %funcall-with-api-handling (fn)
  (log:trace "Got parameters: ~s" (hunchentoot:post-parameters hunchentoot:*request*))
  (let ((api-key (hunchentoot:parameter "api-key"))
        (api-secret-key (hunchentoot:parameter "api-secret-key"))
        (testp (boundp '*current-api-key*)))
    (let ((*current-api-key*
           (if testp
               *current-api-key* ;; only for tests
               (%find-api-key  api-key))))
      (unless (current-user)
       (error 'api-error
              :message (format nil "API key appears to be invalid or non-existant, got: ~a" api-key)))
      (unless (or
               (not *current-api-key*) ;; setings api
               (or testp
                   (equal api-secret-key (api-key-secret-key *current-api-key*))))
        (error 'api-error
               :message "API secret key doesn't match what we have on record"))
      (let ((res (funcall fn)))
        res))))

(defun %with-error-handling (fn)
  (Setf (hunchentoot:header-out :content-type) "application/json")
  (let ((res
          (json:encode-json-to-string
           (handler-case
               (make-instance 'result
                              :success t
                              :response
                              (%funcall-with-api-handling fn))
            (api-error (e)
              (log:warn "API error: ~a" (api-error-msg e))
              (make-instance 'error-result
                              :success nil
                              :error (princ-to-string e)))))))
    res))

(defmacro defapi ((name &key uri method intern) params &body body)
  (multiple-value-bind (body decls) (tcr.parse-declarations-1.0::parse-body body)
    `(defhandler (,name :uri ,uri :method ,method :intern ,intern :html nil) ,params
       ,@decls
       (flet ((body () ,@body))
         (%with-error-handling #'body)))))

(defclass result ()
  ((success :type boolean
           :initform t
           :initarg :success)
   (response :initarg :response)))

(defclass error-result (result)
  ((error :initarg :error)))

(define-condition api-error (error)
  ((message :initarg :message
            :reader api-error-msg))
  (:report (lambda (e out)
             (format out "~a" (api-error-msg e)))))


(defapi (nil :uri "/api/test") ()
  3)

(defapi (nil :uri "/api/test-error") ()
  (error 'api-error :message "Foo"))

(defclass api-result () ())

(defclass api-response () ())
