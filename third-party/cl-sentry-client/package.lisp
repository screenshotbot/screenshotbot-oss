(defpackage #:sentry-client
  (:use #:cl)
  (:export #:initialize-sentry-client
           #:test-sentry-client
           #:capture-exception
           #:capture-message
           #:forward-event
           #:with-sentry-error-handler
           #:sentry-client
           #:async-sentry-client))
