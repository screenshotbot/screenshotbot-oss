(defpackage :screenshotbot/client-hub/selenium
  (:use #:cl)
  (:import-from #:screenshotbot/hub/server
                #:request-session-and-respond
                #:*hub*)
  (:import-from #:screenshotbot/replay/proxy
                #:replay-proxy)
  (:import-from #:server/interrupts
                #:unwind-on-interrupt)
  (:local-nicknames (#:a #:alexandria)
                    (#:flags #:screenshotbot/client-hub/flags))
  (:export
   #:start-hub))
(in-package :screenshotbot/client-hub/selenium)

(defclass client-hub ()
  ())

(defmethod request-session-and-respond ((hub client-hub)
                                        (arguments string))
  (error "unimpl"))

(defun start-hub ()
  (log:info "Starting selenium hub on ~a" flags:*port*)
  (let ((proxy
          (make-instance 'replay-proxy
                          :hub (make-instance 'client-hub)
                          :cache-dir (ensure-directories-exist #P"~/screenshotbot/proxy-cache/")
                          :port flags:*port*)))
    (unwind-on-interrupt ()
        (hunchentoot:start proxy)
        (progn
          (log:info "terminating")
          (hunchentoot:stop proxy)))))
