(defpackage #:auth
  (:use #:cl)
  (:import-from :bknr.datastore
   :store-object
                :hash-index
                :unique-index
   :persistent-class)
  (:import-from :hunchentoot
                :set-cookie
                :host
                :cookie-in)
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
           #:csrf-token))
