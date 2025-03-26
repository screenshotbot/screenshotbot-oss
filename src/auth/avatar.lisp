;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :auth/avatar
  (:use #:cl)
  (:import-from #:util/misc
                #:?.)
  (:import-from #:auth
                #:oauth-user-avatar)
  (:import-from #:util/request
                #:http-request)
  (:import-from #:encrypt/hmac
                #:verify-hmac)
  (:import-from #:bknr.datastore
                #:blob-pathname
                #:persistent-class
                #:blob)
  (:import-from #:util/store/store
                #:with-class-validation
                #:defindex)
  (:import-from #:util/store/fset-index
                #:fset-unique-index
                #:fset-set-index))
(in-package :auth/avatar)

(defindex +user-index-2+
  'fset-unique-index
  :slot-name '%user)

(defvar *overriden-avatar-lock* (bt:make-lock)
  "When fetching or updating the avatars, this lock must be held. TODO:
does it make sense to parallelize this in the future?")

(with-class-validation
  (defclass overriden-avatar (blob)
    ((content-type :initarg :content-type
                   :reader content-type)
     (%user :initarg :user
            :index +user-index-2+
            :index-reader overriden-avatar-for-user))
    (:metaclass persistent-class)
    (:documentation "If this object is created, the the avatar is overriden for the given
user. Currently only being used by Microsoft Entra, which has specific requirements
for downloading the profile picture.")))


(hex:def-clos-dispatch ((self auth:auth-acceptor-mixin) "/account/avatar") (id signature)
  (assert
   (verify-hmac (format nil "avatar.~a" id)
                (ironclad:hex-string-to-byte-array
                 signature)))
  (let ((user (bknr.datastore:store-object-with-id (parse-integer id))))
    (handle-avatar user)))

(defun handle-avatar (user)
  (bt:with-lock-held (*overriden-avatar-lock*)
    (let ((overriden-avatar (overriden-avatar-for-user user)))
      (cond
        (overriden-avatar
         (hunchentoot:handle-static-file
          (blob-pathname overriden-avatar)
          (content-type overriden-avatar)))
        (t
         (hunchentoot:redirect
          (actual-public-url user)))))))

(defmethod actual-public-url (user)
  (let ((known-avatar
          (?. oauth-user-avatar (car (auth:oauth-users user)))))
    (cond
      ((or
        (str:emptyp known-avatar)
        ;; T1828: temporary hack to use Gravatar for Entra
        (equal "https://graph.microsoft.com/v1.0/me/photo/$value" known-avatar))
       (format nil "~a" (gravatar:image-url
                               (auth:user-email user))))
      (t
       known-avatar))))



