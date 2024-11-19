;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/sdk/test-cli-common
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/sdk/cli-common
                #:root/command)
  (:import-from #:cl-mock
                #:if-called
                #:answer)
  (:import-from #:screenshotbot/sdk/install
                #:credential-file)
  (:import-from #:screenshotbot/sdk/api-context
                #:secret
                #:key
                #:hostname)
  (:import-from #:screenshotbot/sdk/clingon-api-context
                #:make-api-context))
(in-package :screenshotbot/sdk/test-cli-common)


(util/fiveam:def-suite)


(def-fixture state ()
  (cl-mock:with-mocks ()
    (&body)))

(test can-create-root-command
  (with-fixture state ()
   (finishes (root/command))))


