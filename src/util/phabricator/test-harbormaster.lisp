;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/phabricator/test-harbormaster
  (:use #:cl
        #:fiveam)
  (:import-from #:util/phabricator/harbormaster
                #:create-artifact
                #:download-file
                #:upload-file)
  (:import-from #:util/phabricator/conduit
                #:phab-instance
                #:call-conduit
                #:make-phab-instance-from-arcrc)
  (:import-from #:util/mock-recording
                #:recording-mode-p
                #:track
                #:with-recording))
(in-package :util/phabricator/test-harbormaster)

(util/fiveam:def-suite)

(def-fixture state (name &key (record nil))
  (with-recording ((asdf:system-relative-pathname :util/phabricator
                                                  (format nil
                                                          "phabricator/fixture/~a.rec"
                                                          name))
                   :record record)
    (track 'call-conduit
           :skip-args '(0))
    (let ((phab
            (if (recording-mode-p)
                (make-phab-instance-from-arcrc "https://phabricator.tdrhq.com")
                (make-instance 'phab-instance))))
      (&body))))

(test upload-file
  (with-fixture state ("upload-file")
   (uiop:with-temporary-file (:pathname p :stream s :direction :output
                              :element-type 'character
                              :prefix "input-file")
     (write-string "hello world" s)
     (finish-output s)
     (let ((phid (upload-file phab p)))
       (is (stringp phid))
       (uiop:with-temporary-file (:pathname output :prefix "final-download")
         (download-file phab phid output)
         #+nil(is (equal "hello world" (uiop:read-file-string output))))))))

(test create-artifact
  (with-fixture state ("create-artifact")
   (uiop:with-temporary-file (:pathname p :stream s)
     (write-string "hello world" s)
     (finish-output s)
     (finishes
       (create-artifact phab
                        "PHID-HMBT-6fznwctbnptklhw36y63"
                        p :name "Test Artifact 7")))))
