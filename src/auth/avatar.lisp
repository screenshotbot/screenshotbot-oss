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
                #:verify-hmac))
(in-package :auth/avatar)

(hex:def-clos-dispatch ((self auth:auth-acceptor-mixin) "/account/avatar") (id signature)
  (assert
   (verify-hmac (format nil "avatar.~a" id)
                (ironclad:hex-string-to-byte-array
                 signature)))
  (let ((user (bknr.datastore:store-object-with-id (parse-integer id))))
    (hunchentoot:redirect
     (actual-public-url user))))

(defmethod actual-public-url (user)
  (let ((known-avatar
          (?. oauth-user-avatar (car (auth:oauth-users user)))))
    (cond
      ((str:emptyp known-avatar)
       (format nil "~a" (apply 'gravatar:image-url
                               (auth:user-email user))))
      (t
       known-avatar))))



