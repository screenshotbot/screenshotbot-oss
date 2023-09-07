;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/sdk/test-dev
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/sdk/dev
                #:record-run
                #:homedir
                #:%make-run-and-get-id
                #:record/command)
  (:import-from #:cl-mock
                #:if-called)
  (:import-from #:screenshotbot/sdk/sdk
                #:put-run
                #:request)
  (:import-from #:screenshotbot/api/model
                #:*api-version*)
  (:import-from #:screenshotbot/sdk/api-context
                #:remote-version)
  (:local-nicknames (#:dto #:screenshotbot/api/model)))
(in-package :screenshotbot/sdk/test-dev)

(util/fiveam:def-suite)

(def-fixture state ()
  (cl-mock:with-mocks ()
    (if-called 'uiop:quit (lambda (code)
                            (error "exiting with: ~a" code)))
    (tmpdir:with-tmpdir (%homedir)
      (if-called 'homedir (lambda ()
                            %homedir))
      (tmpdir:with-tmpdir (dir)
        (with-open-file (stream (path:catfile dir "foo.png") :direction :output)
          (write-string "dummy" stream))
        (&body)))))

(defclass fake-api-context ()
  ())

(defmethod request ((api-ctx fake-api-context)
                    script &key &allow-other-keys)
  (cond
   ((equal "/api/screenshot" script)
    nil)
   (t
    (error "unimpl"))))

(defmethod remote-version ((api-ctx fake-api-context))
  *api-version*)

(defmethod put-run ((api-ctx fake-api-context)
                    run)
  (make-instance 'dto:run
                 :id "foo"))

(test record-happy-path
  (with-fixture state ()
    (finishes
      (record-run
       (%make-run-and-get-id
        (make-instance 'fake-api-context)
        :directory dir
        :channel "foo")
       "foo"))
    (is (path:-e (path:catfile %homedir ".config/screenshotbot/recordings/foo.json")))))
