(uiop:define-package :screenshotbot/client-hub/flags
  (:use #:cl
        #:com.google.flag)
  (:use-reexport
   #:screenshotbot/sdk/common-flags)
  (:local-nicknames (#:a #:alexandria)))
(in-package :screenshotbot/client-hub/flags)
