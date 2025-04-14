;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/github/repo-push-webhook
  (:use #:cl)
  (:import-from #:screenshotbot/server
                #:defhandler)
  (:import-from #:alexandria
                #:assoc-value)
  (:import-from #:screenshotbot/github/webhook
                #:validate-hmac)
  (:import-from #:util/misc
                #:not-null!))
(in-package :screenshotbot/github/repo-push-webhook)

(defhandler (nil :uri "/github/:companyid/push/:org/:repo") (companyid org repo)
  (let* ((payload (hunchentoot:raw-post-data :force-binary t)))
    (validate-hmac :webhook-secret "foobar23"
                   :data payload
                   :signature (not-null! (hunchentoot:header-in* :x-hub-signature-256)))
    (let* ((payload (json:decode-json-from-string (flex:octets-to-string payload :external-format :utf-8)))
           (Commits (assoc-value payload :Commits)))
      (warn "unimpl ~a" (hunchentoot:headers-in*) ))))



