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
       (http-request
        (endpoint config)
        :content payload
        :method :post)))))
