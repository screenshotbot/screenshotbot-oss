;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/phabricator/test-harbormaster
  (:use #:cl
        #:fiveam)
  (:import-from #:util/phabricator/harbormaster
                #:file-upload-chunk
                #:delete-after-epoch
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
                #:with-recording)
  (:import-from #:cl-mock
                #:answer)
  (:import-from #:local-time
                #:timestamp-to-universal))
(in-package :util/phabricator/test-harbormaster)

(util/fiveam:def-suite)

(def-fixture state (name &key (record nil))
  (with-recording ((asdf:system-relative-pathname :util/phabricator
                                                  (format nil
                                                          "phabricator/fixture/~a.rec"
                                                          name))
                   :record record)
    (track 'local-time:clock-now)
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

(test upload-large-file
  (with-fixture state ("upload-large-file")
    (let ((buf (make-array 1000 :initial-element 0))
          (uploaded 0))
      (cl-mock:if-called 'file-upload-chunk
                         (lambda (phab phid pos seq)
                           ;; don't actually upload the chunk, the
                           ;; recording is too big.
                           (is (eql pos uploaded))
                           (incf uploaded (length seq))
                           (values)))
      (uiop:with-temporary-file (:pathname large-file
                                 :stream stream
                                 :element-type '(unsigned-byte 8))
        (loop for i from 0 below (* 10 1000)
              do (write-sequence buf stream))
        (finish-output stream)
        (let ((phid (upload-file phab large-file
                                 :name (format nil "unnamed ~a"
                                               (timestamp-to-universal (local-time:now))))))
          (is (eql uploaded 10000000)))))))

(test create-artifact
  (with-fixture state ("create-artifact")
    (track 'call-conduit
           :skip-args '(0))
    (track 'upload-file
           :skip-args '(0 1))
    (uiop:with-temporary-file (:pathname p :stream s)
      (write-string "hello world" s)
      (finish-output s)
      (finishes
        (create-artifact phab
                         "PHID-HMBT-6fznwctbnptklhw36y63"
                         p :name "Test Artifact 76")))))
