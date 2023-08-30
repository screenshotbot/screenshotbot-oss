;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/api/test-commit-graph
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/testing
                #:with-test-user)
  (:import-from #:util/store
                #:with-test-store)
  (:import-from #:screenshotbot/api/commit-graph
                #:%update-commit-graph-v2)
  (:local-nicknames (#:a #:alexandria)))
(in-package :screenshotbot/api/test-commit-graph)


(util/fiveam:def-suite)

(test update-commit-graph-happy-path ()
  (with-test-store ()
   (with-test-user (:logged-in-p t)
     (let ((dag (make-instance 'dag:dag)))
       (dag:add-commit dag (make-instance 'dag:commit
                                          :sha "abcd"
                                          :author "zoidberg") )
       (finishes
        (%update-commit-graph-v2 "https://github.com/foo/bar"
                                 (with-output-to-string (out)
                                   (dag:write-to-stream dag out))))))))
