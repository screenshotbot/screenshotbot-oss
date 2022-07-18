(defpackage :screenshotbot/sdk/selenium
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria)
                    (#:flags #:screenshotbot/sdk/flags))
  (:export
   #:start-hub))
(in-package :screenshotbot/sdk/selenium)


(defun start-hub ()
  (log:info "Starting selenium hub on ~a" flags:*selenium-hub-port*))
