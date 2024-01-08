;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage #:auth
  (:use #:cl)
  (:import-from :bknr.datastore
                :delete-object
                :with-transaction
                :class-instances
   :store-object
                :hash-index
                :unique-index
   :persistent-class)
  (:import-from :hunchentoot
                :set-cookie
                :host
                :cookie-in)
  (:import-from #:util/store/store
                #:defindex
                #:with-class-validation)
  (:import-from #:util/store/store-migrations
                #:def-store-migration)
  (:import-from #:util/store/fset-index
                #:index-least
                #:fset-set-index)
  (:import-from #:util/cron
                #:def-cron)
  (:export #:user-session
           #:session-value
           #:session-key
           #:set-session-user-id
           #:has-session?
           #:drop-session
           #:*current-session*
           #:password-hash
           #:session=
           #:login-page
           #:wrong-password-page
           #:%session-token ;; avoid using
           #:has-password-p
           #:set-session-cookie
           #:current-session
           #:define-login-handlers
           #:check-password
           #:with-sessions
           #:user-password
           #:signup-page
           #:user-class
           #:signup-errors
           #:send-signup-email
           #:current-user-id
           #:%make-session
           #:handle-signup
           #:user-id
           #:current-user
           #:authenticate-request
           #:authenticated-request
           #:find-user-session-value-by-hash
           #:request-user
           #:request-account
           #:csrf-token
           #:current-user
           #:logged-in-p
           #:can-view
           #:can-view!
           #:can-edit
           #:can-edit!
           #:can-public-view
           #:no-access-error
           #:current-company
           #:auth-acceptor-mixin))
