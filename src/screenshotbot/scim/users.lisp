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
                #:set-user-activep
                #:make-scim-user
                #:scim-user-user
                #:scim-user-activep
                #:scim-user-emails
                #:scim-user-external-id
                #:scim-user-company
                #:scim-users-for-company
                #:scim-user-user-name
                #:scim-user-v2
                #:scim-config-company
                #:scim-config-for-token
                #:scim-config-token
                #:scim-configs-for-company)
  (:import-from #:util/misc
                #:not-null!)
  (:import-from #:screenshotbot/scim/dto
                #:external-user-id
                #:user-to-dto
                #:external-user-activep
                #:external-user-external-id
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
                #:find-by-oid
                #:oid)
  (:import-from #:util/json-mop
                #:json-mop-to-string)
  (:import-from #:screenshotbot/api/model
                #:encode-json
                #:decode-json)
  (:import-from #:hunchentoot
                #:*request*)
  (:import-from #:easy-macros
                #:def-easy-macro)
  (:import-from #:screenshotbot/scim/filter
                #:make-filter)
  (:import-from #:screenshotbot/login/signup
                #:valid-email-address-p))
(in-package :screenshotbot/scim/users)

(defvar *lock* (bt:make-lock))

(defun wrap-handlers (callback)
  (encode-json
   (with-api-error-handling ()
     (funcall callback))))

(defmacro defscimhandler ((name &key uri method) params &body body)
  (assert method)
  `(progn
     (defhandler (,name :uri ,uri :method ,method)  ,params
      (wrap-handlers
       (lambda ()
         ,@body)))))

(defun bearer-token ()
  (let ((header (hunchentoot:header-in* :authorization)))
    (unless header
      (error 'access-denied
             :reason "Authorization header missing"))
    (destructuring-bind (type &optional token)
        (str:split " " (str:trim header) :limit 2)
      (unless (string-equal "bearer" type)
        (error 'access-denied :reason "Only bearer supported"))
      (unless token
        (error 'access-denied
               :reason "no token provided"))
      (str:trim token))))

(defun get-company! ()
  (let ((token (bearer-token)))
    (let ((scim-config (scim-config-for-token (not-null! token))))
      (unless scim-config
        (error 'access-denied :reason "no such token"))
      (let ((company (scim-config-company scim-config)))
        (unless company
          (error 'access-denied))
       company))))

(defscimhandler (nil :uri "/scim/v2/Users" :method :get) (filter)
  (let ((company (get-company!)))
    (%list-users company filter)))

(defun %list-users (company filter)
  (let ((filter (cond
                  ((str:emptyp filter)
                   (lambda (user) (declare (ignore user)) t))
                  (t
                   (make-filter filter))))
        (start-index (max
                      (parse-integer
                       (or
                        (hunchentoot:parameter "startIndex")
                        "1"))
                      1))
        (count (parse-integer
                (or
                 (hunchentoot:parameter "count")
                 "100"))))
    (let* ((users (fset:convert 'list (scim-users-for-company company)))
           (users (remove-if-not filter users)))
      (set-success 200)
      (let ((resources
              (loop for user in users
                    for i from 1
                    if (and
                        (<= start-index i)
                        (< i (+ start-index count)))
                      collect
                      user)))
        (make-instance
         'list-response
         :total-results (length users)
         :start-index start-index
         :items-per-page (length resources)
         :resources
         resources)))))

(define-condition api-error (error)
  ((code :initarg :code
         :reader api-error-code)
   (scim-type :initarg :type
              :initform nil
              :reader api-error-type)
   (reason :initarg :reason
           :initform "NA"
           :reader api-error-reason)))

(defmethod print-object ((self api-error) output)
  (format output "API-ERROR: ~a"
          (api-error-reason self)))

(define-condition invalid-value (api-error)
  ()
  (:default-initargs :code 400 :type "invalidValue"))

(defun invalid-value (&rest args)
  (error 'invalid-value
         :reason (apply #'format nil args)))

(define-condition user-name-must-be-email (api-error)
  ()
  (:default-initargs :code 400 :type "invalidValue" :reason "We expect the username to be the same as the email"))

(define-condition only-one-email (api-error)
  ()
  (:default-initargs :code 400 :type "invalidValue" :reason "Exactly one email is allowed per user"))

(define-condition does-not-exist (api-error)
  ()
  (:default-initargs :code 404 :type nil))

(define-condition uniqueness-error (api-error)
  ()
  (:default-initargs :code 409 :type "uniqueness"))

(define-condition access-denied (api-error)
  ()
  (:default-initargs :code 403 :type nil))

(define-condition invalid-email (api-error)
  ()
  (:default-initargs :code 400 :type "invalidValue" :reason "invalid email"))

(def-easy-macro with-api-error-handling (&fn fn)
  (handler-case
      (fn)
    (api-error (e)
      (set-success) ;; we'll override this in the next line!
      (setf (hunchentoot:return-code*) (api-error-code e))
      (make-instance 'error-response
                     :type (api-error-type e)
                     :detail (api-error-reason e)
                     :status (coerce
                              (format nil "~a" (api-error-code e))
                              'vector)))))



(defun set-success (&optional (code 201))
  (setf (hunchentoot:return-code*) code) ;; SCIM requires this
  (setf (hunchentoot:content-type*) "application/scim+json"))

(defscimhandler (nil :uri "/scim/v2/Users" :method :post) ()
  (with-api-error-handling ()
    (let ((company (get-company!)))
      (let ((response (scim-post company (hunchentoot:raw-post-data :force-text t))))
       (set-success)
       response))))

(defun scim-post (company json)
  (bt:with-lock-held (*lock*)
    (let ((dto (decode-json
                 json
                 'external-user)))
      (validate-dto dto)
      (let* ((username (external-user-user-name dto)))
        (dolist (existing-user (scim-users-for-company company))
         (when (equal (external-user-user-name existing-user)
                      username)
           (error 'uniqueness-error)))
       (let ((obj (make-scim-user
                   :company company
                   :user-name username
                   :activep (external-user-activep dto)
                   :external-id (ignore-errors
                                 (external-user-external-id dto))
                   :emails (dto-emails dto))))
         (setf (hunchentoot:header-out :location)
               (hex:make-full-url *request*
                                  "/scim/v2/Users/:id"
                                  :id (external-user-id  obj)))
         obj)))))

(defun validate-dto (dto)
  (let ((external-emails (dto-emails dto)))
    (unless (eql 1 (length external-emails))
      (error 'only-one-email))
    (unless (equal
             (external-user-user-name dto)
             (external-email-value (car (external-user-emails dto))))
      (error 'user-name-must-be-email))
    (unless (valid-email-address-p (external-user-user-name dto))
      (error 'invalid-email))))


(defscimhandler (nil :uri "/scim/v2/Users/:id" :method :get) (id)
  (set-success 200)
  (scim-get (get-company!) id))

(defun validate-user! (company user)
  ;; In a previous version, we were validating the company of the
  ;; user. However, the DTO model does not carry this information, and
  ;; the fetching code always takes the company... 
  (declare (ignore company))
  (unless user
    (error 'does-not-exist))
  (check-type user external-user))

(defun scim-get (company id)
  (let ((user (scim-user-by-id company id)))
    (validate-user! company user)
    user))

(defscimhandler (nil :uri "/scim/v2/Users/:id" :method :delete) (id)
  (error 'api-error :code 501 :reason  "DELETE is not supported, send active=false instead"))


(defscimhandler (nil :uri "/scim/v2/Users/:id" :method :put) (id)
  (log:info "PUT new user for SCIM")
  (scim-put (get-company!) id
              (hunchentoot:raw-post-data :force-text t)))

(defscimhandler (nil :uri "/scim/v2/Users/:id" :method :patch) (id)
  (log:info "PATCH user for SCIM")
  (error 'api-error :code 501 :reason "PATCH not supported for SCIM"))

(defun scim-user-by-id (company id)
  (loop for scim-user in (scim-users-for-company company)
        if (equal id (external-user-id scim-user))
          return scim-user))

(defun scim-put (company id json)
  (bt:with-lock-held (*lock*)
    (let ((dto (decode-json
                json
                'external-user)))
      (validate-dto dto)
      (let* ((username (external-user-user-name dto))
             (existing (scim-user-by-id company id))
             (internal-user
               (loop for user in (roles:users-for-company company)
                     if (equal id (oid user))
                       return user)))
        (validate-user! company existing)
        (dolist (existing-user (scim-users-for-company company))
          (when (and
                 (not (equal (external-user-id existing) id))
                 (equal (external-user-user-name existing-user)
                        username))
            (error 'uniqueness-error)))
        #+nil
        (setf (scim-user-user-name existing)
              username)
        #+nil
        (setf (scim-user-emails existing)
              (dto-emails dto))
        #+nil
        (setf (scim-user-external-id existing)
              (ignore-errors (external-user-external-id dto)))

        (set-user-activep
         company
         internal-user
         (external-user-activep dto))


        (set-success 200)
        existing))))

(defun dto-emails (dto)
  "Get a list of all the emails from the DTO"
  (loop for email in (external-user-emails
                      dto)
        collect
        (external-email-value email)))



