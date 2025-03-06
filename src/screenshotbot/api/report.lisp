;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/api/report
  (:use #:cl)
  (:import-from #:screenshotbot/api/core
                #:defapi)
  (:import-from #:screenshotbot/report-api
                #:report-acceptable)
  (:import-from #:alexandria
                #:when-let)
  (:import-from #:screenshotbot/model/report
                #:report-to-dto
                #:acceptable-state))
(in-package :screenshotbot/api/report)

(defapi (%report-accept :uri "/api/report/:oid/review/accept"
                        :method :post
                        :wrap-success t)
        (oid)
  (let ((report (util:find-by-oid oid)))
    (auth:can-view! report)
    (when-let ((acceptable (report-acceptable report)))
      (setf (acceptable-state acceptable) :accepted))
    (report-to-dto report)))




