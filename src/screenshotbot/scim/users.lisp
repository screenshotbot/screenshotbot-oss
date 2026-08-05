;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/scim/users
  (:use #:cl)
  (:import-from #:screenshotbot/server
                #:defhandler)
  (:import-from #:screenshotbot/scim/model
                #:scim-users-for-company
                #:scim-user-user-name
                #:scim-user
                #:scim-config-company
                #:scim-config-for-token
                #:scim-config-token
                #:scim-configs-for-company)
  (:import-from #:util/misc
                #:not-null!)
  (:import-from #:screenshotbot/scim/dto
                #:error-response
                #:external-user-user-name
                #:external-user-name
                #:external-email-value
                #:external-user-emails
                #:list-response
                #:external-user
                #:external-email)
  (:import-from #:auth
                #:can-viewer-view
                #:user-email)
  (:import-from #:util/store/object-id
                #:oid)
  (:import-from #:util/json-mop
                #:json-mop-to-string)
  (:import-from #:screenshotbot/api/model
                #:encode-json
                #:decode-json)
  (:import-from #:hunchentoot
                #:*request*)
  (:import-from #:easy-macros
                #:def-easy-macro))
(in-package :screenshotbot/scim/users)

(defvar *lock* (bt:make-lock))

(defun bearer-token ()
  (let ((header (hunchentoot:header-in* :authorization)))
    (destructuring-bind (type token)
        (str:split " " (str:trim header) :limit 2)
      (unless (string-equal "bearer" type)
        (error "Only bearer token supported"))
      (str:trim token))))

(defun get-company! ()
  (let ((token (bearer-token)))
   (let ((scim-config (not-null! (scim-config-for-token (not-null! token)))))
     (not-null! (scim-config-company scim-config)))))

(defhandler (nil :uri "/scim/v2/Users" :method :get) ()
  (let ((company (get-company!)))
    (let ((users (roles:users-for-company company)))
      (json-mop-to-string
       (make-instance
        'list-response
        :total-results (length users)
        :start-index 1
        :items-per-page (length users)
        :resources
        (loop for user in users
              collect
              (make-instance 'external-user
                             :id (format nil "~a" (bknr.datastore:store-object-id user))
                             :user-name (user-email user)
                             :emails (list (make-instance 'external-email
                                                          :type "work"
                                                          :value (user-email user))))))))))

(define-condition api-error ()
  ((code :initarg :code
         :reader api-error-code)
   (scim-type :initarg :type
              :reader api-error-type)))

(define-condition uniqueness-error (api-error)
  ()
  (:default-initargs :code 409 :type "uniqueness"))

(def-easy-macro with-api-error-handling (&fn fn)
  (handler-case
      (fn)
    (api-error (e)
      (set-success) ;; we'll override this in the next line!
      (setf (hunchentoot:return-code*) (api-error-code e))
      (json-mop-to-string
       (make-instance 'error-response
                      :type (api-error-type e)
                      :status (format nil "~a" (api-error-code e)))))))

(defun set-success ()
  (setf (hunchentoot:return-code*) 201) ;; SCIM requires this
  (setf (hunchentoot:content-type*) "application/scim+json"))

(defhandler (nil :uri "/scim/v2/Users" :method :post) ()
  (with-api-error-handling ()
    (let ((company (get-company!)))
      (let ((response (scim-post company (hunchentoot:raw-post-data :force-text t))))
       (set-success)
       response))))

(defmethod user-to-dto ((user scim-user))
  (make-instance 'external-user
                 :id (format nil "~a" (bknr.datastore:store-object-id user))
                 :user-name (scim-user-user-name user)))


(defun scim-post (company json)
  (bt:with-lock-held (*lock*)
    (let* ((dto (decode-json
                 json
                 'external-user))
           (username (external-user-user-name dto)))
      (fset:do-set (existing-user (scim-users-for-company company))
        (when (equal (scim-user-user-name existing-user)
                     username)
          (error 'uniqueness-error)))
      (let ((obj (make-instance 'scim-user
                                :company company
                                :user-name username
                                :emails (loop for email in (external-user-emails
                                                            dto)
                                              collect
                                              (external-email-value email)))))
        (setf (hunchentoot:header-out :location)
              (hex:make-full-url *request*
                                 "/scim/v2/Users/:id"
                                 :id (bknr.datastore:store-object-id  obj)))
        (encode-json
         (user-to-dto
          obj))))))

