;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/webhook/webhook
  (:use #:cl)
  (:import-from #:screenshotbot/api/model
                #:encode-json)
  (:import-from #:screenshotbot/webhook/model
                #:signing-key
                #:endpoint
                #:webhook-config-for-company)
  (:import-from #:util/request
                #:http-request)
  (:import-from #:alexandria
                #:when-let*)
  (:import-from #:util/threading
                #:make-thread)
  (:export
   #:send-webhook))
(in-package :screenshotbot/webhook/webhook)

(defmethod send-webhook (company payload)
  (when-let* ((payload (encode-json payload))
              (config (webhook-config-for-company company)))
    (make-thread
     (lambda ()
       (log:info "Sending payload")
       (let ((signature (sign-payload payload :key (signing-key config))))
         (http-request
          (endpoint config)
          :content payload
          :additional-headers `(("Screenshotbot-Signature"
                                 signature))
          :method :post))))))

(defun sign-payload (payload &key (time (local-time:now))
                               key)
  (let* ((unix (local-time:timestamp-to-unix time))
         (payload (format nil "~a.~a"
                          unix
                          payload))
         (mac (ironclad:make-mac :hmac
                                 (flex:string-to-octets key)
                                 :sha256)))
    (ironclad:update-mac mac (flex:string-to-octets payload))
    (format nil
            "t=~a,signature=~a"
            unix
            (ironclad:byte-array-to-hex-string (ironclad:produce-mac mac)))))
