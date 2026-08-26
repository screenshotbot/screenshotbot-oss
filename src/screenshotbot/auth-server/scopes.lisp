;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/auth-server/scopes
  (:use #:cl)
  (:export
   #:oauth-scope
   #:scope-name
   #:scope-label
   #:scope-permissions
   #:*supported-scopes*
   #:supported-scope-names
   #:find-scope
   #:parse-scope-string
   #:render-scope-list
   #:scopes-permissions
   #:default-scopes))
(in-package :screenshotbot/auth-server/scopes)

(defclass oauth-scope ()
  ((name :initarg :name
         :reader scope-name
         :documentation "The scope token as it appears on the wire, e.g. \"api:read\".")
   (label :initarg :label
          :reader scope-label
          :documentation "Human readable text shown on the consent screen.")
   (permissions :initarg :permissions
                :initform nil
                :reader scope-permissions
                :documentation "The API-KEY-PERMISSIONS granted by this scope. See
SCREENSHOTBOT/DASHBOARD/API-KEY-IMPL:API-KEY-AVAILABLE-PERMISSIONS."))
  (:documentation "One OAuth 2.0 scope that a client may request."))

(defparameter *supported-scopes*
  (list
   (make-instance 'oauth-scope
                  :name "profile"
                  :label "Read your name and email address"
                  :permissions nil)
   (make-instance 'oauth-scope
                  :name "api:read"
                  :label "Read your runs, channels and reports"
                  :permissions '(:full))
   (make-instance 'oauth-scope
                  :name "api:write"
                  :label "Upload new runs and screenshots on your behalf"
                  :permissions '(:ci)))
  "Every scope this authorization server will issue.")

(defun supported-scope-names ()
  (mapcar #'scope-name *supported-scopes*))

(defun default-scopes ()
  "The scopes granted when a client doesn't ask for anything specific.

RFC 6749 leaves this up to the server, so we pick the least privileged
useful set."
  (list "profile"))

(defun find-scope (name)
  (find name *supported-scopes* :key #'scope-name :test #'equal))

(defun parse-scope-string (scope)
  "Parse a space delimited SCOPE string into a list of scope names.

Returns two values: the list of recognized scope names, and the list of
unrecognized ones. Duplicates are collapsed and ordering follows
*SUPPORTED-SCOPES* so that the consent screen is stable."
  (let* ((requested (remove-if #'str:emptyp
                               (str:split " " (str:trim (or scope "")) :omit-nulls t)))
         (requested (remove-duplicates requested :test #'equal)))
    (values
     (loop for scope in *supported-scopes*
           if (member (scope-name scope) requested :test #'equal)
             collect (scope-name scope))
     (remove-if #'find-scope requested))))

(defun render-scope-list (scopes)
  "Render a list of scope names back into the space delimited wire format."
  (str:join " " scopes))

(defun scopes-permissions (scopes)
  "The union of the API key permissions implied by SCOPES."
  (remove-duplicates
   (loop for name in scopes
         for scope = (find-scope name)
         if scope
           append (scope-permissions scope))))
