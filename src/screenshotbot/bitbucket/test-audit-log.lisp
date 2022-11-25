;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/bitbucket/test-audit-log
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/bitbucket/audit-log
                #:audit-log-error
                #:parse-error-response
                #:audit-log)
  (:import-from #:util/store
                #:with-test-store)
  (:import-from #:screenshotbot/pro/bitbucket/core
                #:bitbucket-error)
  (:import-from #:screenshotbot/pro/bitbucket/audit-log
                #:with-audit-log)
  (:import-from #:bknr.datastore
                #:class-instances)
  (:import-from #:fiveam-matchers/core
                #:equal-to
                #:assert-that)
  (:import-from #:fiveam-matchers/has-length
                #:has-length)
  (:import-from #:fiveam-matchers/misc
                #:is-null)
  (:import-from #:fiveam-matchers/strings
                #:contains-string
                #:starts-with)
  (:import-from #:bknr.datastore
                #:with-transaction)
  (:local-nicknames (#:a #:alexandria)))
(in-package :screenshotbot/bitbucket/test-audit-log)


(util/fiveam:def-suite)

(def-fixture state ()
  (with-test-store ()
    (&body)))

(test test-parses-error-correctly
  (with-fixture state ()
   (let ((response (uiop:read-file-string
                    (asdf:system-relative-pathname
                     :screenshotbot "bitbucket/error-response-1.json")))
         (audit-log (make-instance 'audit-log)))
     (signals bitbucket-error
      (parse-error-response
       response
       500
       audit-log))
     (is (equal
          "key: Ensure this value has at most 40 characters (it has 44)."
          (audit-log-error audit-log))))))
