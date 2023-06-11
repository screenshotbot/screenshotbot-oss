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
                #:event-payload
                #:config-company
                #:webhook-payload
                #:event
                #:webhook-event
                #:enabledp
                #:signing-key
                #:endpoint
                #:webhook-config-for-company)
  (:import-from #:util/request
                #:http-request)
  (:import-from #:alexandria
                #:assoc-value
                #:when-let*)
  (:import-from #:util/threading
                #:make-thread)
  (:import-from #:screenshotbot/audit-log
                #:audit-log-company
                #:with-audit-log)
  (:import-from #:screenshotbot/model/company
                #:company)
  (:export
   #:send-webhook))
(in-package :screenshotbot/webhook/webhook)

(defmethod send-webhook (company (payload webhook-payload))
  (when-let* ((event-name (event payload))
              (payload (encode-json payload))
              (config (webhook-config-for-company company)))
    (when (enabledp config)
     (make-thread
      (lambda ()
        (actually-send-webhook config event-name payload))))))

(defmethod force-resend-webhook ((ev webhook-event))
  (when-let* ((company (audit-log-company ev))
              (config (webhook-config-for-company company)))
    (actually-send-webhook config (assoc-value
                                   (json:decode-json-from-string (event-payload ev))
                                   :event)
                           (event-payload ev))))

(defmethod actually-send-webhook (config event-name (payload string))
  (log:info "Sending payload")
  (ignore-errors
   (with-audit-log (audit-log (make-instance 'webhook-event
                                             :company (config-company config)
                                             :event event-name
                                             :payload payload))
     (declare (ignore audit-log))
     (let ((signature (sign-payload payload :key (signing-key config))))
       (http-request
        (endpoint config)
        :content payload
        :additional-headers `(("Screenshotbot-Signature"
                               . ,signature))
        :ensure-success t
        :method :post)))))

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
