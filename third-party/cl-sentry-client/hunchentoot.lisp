(in-package :sentry-client)

(defmethod hunchentoot:maybe-invoke-debugger :after (condition)
  (when hunchentoot:*catch-errors-p*
    #-sbcl(sentry-client:capture-exception condition)
    #+sbcl
    (let ((sb-debug:*stack-top-hint* nil))
      (sentry-client:capture-exception condition))))
