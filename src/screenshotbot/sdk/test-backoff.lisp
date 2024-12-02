;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/sdk/test-backoff
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/sdk/backoff
                #:server-unavailable
                #:too-many-requests
                #:maybe-retry-request))
(in-package :screenshotbot/sdk/test-backoff)


(util/fiveam:def-suite)

(test maybe-retry-request-happy-path
  "Just that for each of the codes, we correctly invoke the restart,
since the logs might be different for each code."
  (loop for code in '(429 502 503) do
    (is
     (eql :done
          (restart-case
              (maybe-retry-request code
                                   :attempt 0
                                   :backoff 0
                                   :restart 'foobar)
            (foobar ()
              :done))))))

(test if-weve-attempted-enough-times-crash
  (dolist (code '(429 503))
   (signals too-many-requests
     (restart-case
         (maybe-retry-request
          code
          :attempt 5
          :backoff 0
          :restart 'foobar)
       (foobar ()
         :done)))))

(test if-weve-attempted-enough-times-crash-for-service-unavailable
  (signals server-unavailable
    (restart-case
        (maybe-retry-request
         502
         :attempt 5
         :backoff 0
         :restart 'foobar)
      (foobar ()
        :done))))



