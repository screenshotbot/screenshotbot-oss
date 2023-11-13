;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/sdk/test-cli-common
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/sdk/cli-common
                #:make-api-context
                #:root/command)
  (:import-from #:cl-mock
                #:if-called
                #:answer)
  (:import-from #:screenshotbot/sdk/install
                #:credential-file)
  (:import-from #:screenshotbot/sdk/api-context
                #:secret
                #:key
                #:hostname))
(in-package :screenshotbot/sdk/test-cli-common)


(util/fiveam:def-suite)


(def-fixture state ()
  (cl-mock:with-mocks ()
    (&body)))

(test can-create-root-command
  (with-fixture state ()
   (finishes (root/command))))

(test make-api-context-reads-from-credential-file
  (with-fixture state ()
   (uiop:with-temporary-file (:pathname p :stream out)
     (write-string "{\"hostname\":\"foo\",\"apiKey\":\"bar\",\"apiSecretKey\":\"car\"}" out)
     (finish-output out)

     (if-called 'credential-file
                (lambda () p))
     (let ((res (make-api-context :api-key nil
                                  :api-secret nil)))
       (is (equal "foo" (hostname res)))
       (is (equal "bar" (key res)))
       (is (equal "car" (secret res)))))))
