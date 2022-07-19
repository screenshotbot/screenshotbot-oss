(defpackage :screenshotbot/sdk/selenium
  (:use #:cl)
  (:import-from #:screenshotbot/hub/server
                #:request-session-and-respond
                #:*hub*)
  (:import-from #:screenshotbot/replay/proxy
                #:replay-proxy)
  (:local-nicknames (#:a #:alexandria)
                    (#:flags #:screenshotbot/sdk/flags))
  (:export
   #:start-hub))
(in-package :screenshotbot/sdk/selenium)

(defclass client-hub ()
  ())

(defmethod request-session-and-respond ((hub client-hub)
                                        (arguments string))
  (error "unimpl"))

(defun start-hub ()
  (log:info "Starting selenium hub on ~a" flags:*selenium-hub-port*)
  (let ((proxy
          (make-instance 'replay-proxy
                          :hub (make-instance 'client-hub)
                          :cache-dir (ensure-directories-exist #P"~/screenshotbot/proxy-cache/")
                          :port flags:*selenium-hub-port*)))
    (hunchentoot:start proxy)))
