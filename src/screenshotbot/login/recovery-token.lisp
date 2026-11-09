;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/login/recovery-token
  (:use #:cl)
  (:import-from #:bknr.datastore
                #:store-object
                #:persistent-class)
  (:import-from #:util/store/store
                #:defindex)
  (:import-from #:util/store/fset-index
                #:fset-set-index
                #:fset-unique-index)
  (:export
   #:recovery-token
   #:make-recovery-token
   #:find-recovery-token
   #:recovery-token-company
   #:recovery-token-note
   #:recovery-token-expires-at
   #:recovery-token-expired-p
   #:recovery-tokens-for-company))
(in-package :screenshotbot/login/recovery-token)

(defparameter *default-expires-in* (* 24 3600))

(defindex +hash-index+
  'fset-unique-index
  :slot-name '%token-hash)

(defindex +company-index+
  'fset-set-index
  :slot-name '%company)

(defclass recovery-token (store-object)
  ((%token-hash :initarg :token-hash
                :index +hash-index+
                :index-reader %recovery-token-for-hash
                :documentation "SHA-256 of the token, in hex. We never store the token
itself, so a leaked snapshot can't be used to log in.")
   (%company :initarg :company
             :index +company-index+
             :index-reader recovery-tokens-for-company
             :reader recovery-token-company)
   (note :initarg :note
         :initform nil
         :reader recovery-token-note
         :documentation "Who asked for it and why, e.g. a support ticket")
   (created-at :initform (get-universal-time)
               :reader recovery-token-created-at)
   (expires-at :initarg :expires-at
               :reader recovery-token-expires-at))
  (:metaclass persistent-class)
  (:documentation "A short-lived token that a support rep creates by hand for a
company, to get back in when their SSO is broken (e.g. the IdP
certificate rotated).

TODO: logging in with it isn't implemented yet. When it is, decide
whether it's single use, which user it logs in as (it should only ever
be an admin of RECOVERY-TOKEN-COMPANY), and add it to the audit log."))

(defun hash-token (token)
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence
    :sha256
    (flexi-streams:string-to-octets token :external-format :utf-8))))

(defun generate-token ()
  (base64:usb8-array-to-base64-string
   (secure-random:bytes 32 secure-random:*generator*)
   :uri t))

(defun make-recovery-token (company &key (expires-in *default-expires-in*) note)
  "Create a recovery token for COMPANY. Returns the RECOVERY-TOKEN and, as
a second value, the token string to send to the customer. The token
string can't be recovered later."
  (check-type expires-in (integer 1))
  (let ((token (generate-token)))
    (values
     (make-instance 'recovery-token
                    :token-hash (hash-token token)
                    :company company
                    :note note
                    :expires-at (+ (get-universal-time) expires-in))
     token)))

(defun recovery-token-expired-p (self &key (now (get-universal-time)))
  (>= now (recovery-token-expires-at self)))

(defun find-recovery-token (token)
  "The unexpired RECOVERY-TOKEN for the token string TOKEN, or NIL."
  (when (str:non-empty-string-p token)
    (let ((self (%recovery-token-for-hash (hash-token token))))
      (when (and self
                 (not (recovery-token-expired-p self)))
        self))))
