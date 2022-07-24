(uiop:define-package :screenshotbot/client-hub/flags
  (:use #:cl
        #:com.google.flag)
  (:use-reexport
   #:screenshotbot/sdk/common-flags)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:*port*))
(in-package :screenshotbot/client-hub/flags)

(define-flag *port*
  :selector "port"
  :default-value 4444
  :type integer)
