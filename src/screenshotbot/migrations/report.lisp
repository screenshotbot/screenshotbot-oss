;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/migrations/report
  (:use #:cl)
  (:import-from #:util/store/store-migrations
                #:def-store-migration)
  (:import-from #:bknr.datastore
                #:with-transaction
                #:class-instances)
  (:import-from #:screenshotbot/model/company
                #:company)
  (:import-from #:screenshotbot/user-api
                #:company-reports)
  (:import-from #:screenshotbot/model/report
                #:promotion-report-p))
(in-package :screenshotbot/migrations/report)

(def-store-migration ("Update promotion-report-p" :version 3)
  (dolist (company (class-instances 'company))
    (log:info "Updating reports for ~s" company)
    (dolist (report (company-reports company))
      (with-transaction ()
        (setf (slot-value report 'promotion-report-p) t)))))
