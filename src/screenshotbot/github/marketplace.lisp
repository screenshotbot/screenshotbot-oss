;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(uiop:define-package :screenshotbot/github/marketplace
  (:use #:cl #:alexandria)
  (:import-from #:bknr.datastore
                #:persistent-class)
  (:import-from #:util
                #:object-with-oid
                #:oid)
  (:import-from #:screenshotbot/server
                #:defhandler)
  (:export #:github-marketplace-event))
(in-package :screenshotbot/github/marketplace)



(defclass github-marketplace-event (object-with-oid)
  ((payload :initarg :payload
            :reader %payload)
   (secret :initarg :secret)
   (signature :initarg :signature)
   (verified-p :initform nil)
   (created-at :initform (get-universal-time)))
  (:metaclass persistent-class))

(defmethod payload ((ev github-marketplace-event))
  (json:decode-json-from-string (%payload ev)))

(defhandler (nil :uri "/gh-marketplace-event" :method :post) (payload secret)
  (let ((event (make-instance 'github-marketplace-event
                              :payload payload
                              :signature (hunchentoot:header-in* "X-Hub-Signature-256" )
                              :secret secret)))
    (log:info "Got marketplace-event: ~a" (oid event))
    "OK"))
