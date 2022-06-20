;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/web-build/test-project
  (:use #:cl
        #:fiveam)
  (:import-from #:util/store
                #:with-test-store)
  (:import-from #:screenshotbot/web-build/project
                #:web-project-scheduled-job
                #:update-scheduled-job
                #:web-project)
  (:import-from #:bknr.datastore
                #:object-destroyed-p)
  (:local-nicknames (#:a #:alexandria)))
(in-package :screenshotbot/web-build/test-project)


(util/fiveam:def-suite)

(def-fixture state ()
  (with-test-store ()
    (&body)))

(test update-scheduled-job
  (with-fixture state ()
    (let ((project (make-instance 'web-project :name "foobar")))
      (update-scheduled-job project t "*/3 * * *")
      (let ((first-job (web-project-scheduled-job project)))
        (is-true first-job)
        (update-scheduled-job project t "*/4 * * *")
        (let ((second-job (web-project-scheduled-job project)))
          (is-true second-job)
          (is (not (eql first-job second-job)))
          (is (object-destroyed-p first-job)))))))
