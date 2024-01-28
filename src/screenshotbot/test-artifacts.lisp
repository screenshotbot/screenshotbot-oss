;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/test-artifacts
  (:use :cl)
  (:import-from #:it.bese.fiveam
                #:def-fixture
                #:finishes
                #:is
                #:test
                #:with-fixture)
  (:import-from #:screenshotbot/artifacts
                #:*artifact-hooks*
                #:*in-test-p*
                #:artifact-link
                #:call-hooks
                #:def-artifact-hook)
  (:import-from #:screenshotbot/testing
                #:with-installation)
  (:import-from #:util/store/store
                #:*object-store*
                #:object-store)
  (:import-from #:util/testing
                #:with-local-acceptor)
  (:local-nicknames (#:a #:alexandria)))
(in-package :screenshotbot/test-artifacts)


(util/fiveam:def-suite)

(def-fixture state ()
  (tmpdir:with-tmpdir (dir)
    (assert (not (boundp '*object-store*)))
    (let ((old-hooks *artifact-hooks*))
      (with-installation (:globally t)
       (unwind-protect
            (progn
              (setf *object-store* (namestring dir))
              (setf old-hooks *artifact-hooks*)
              (setf *in-test-p* t)
              (with-local-acceptor (host) ('screenshotbot/server:acceptor)
                (&body)))
         (setf *in-test-p* nil)
         (setf *artifact-hooks* old-hooks)
         (makunbound '*object-store*))))))

(test upload-and-download ()
  (with-fixture state ()
    (with-open-file (output
                     (ensure-directories-exist
                      (path:catfile
                       (object-store)
                       "artifacts/test-art"))
                     :direction :output)
      (write-string "hello" output)
      (finish-output output)
      (is
       (equal "hello"
              (util/request:http-request
               (format nil "~a/artifact/test-art" host)
               :want-string t))))))


(test upload-and-download-with-ext ()
  (with-fixture state ()
    (with-open-file (output
                     (ensure-directories-exist
                      (path:catfile
                       (object-store)
                       "artifacts/test-art.txt"))
                     :direction :output)
      (write-string "hello" output)
      (finish-output output)
      (is
       (equal "hello"
              (util/request:http-request
               (format nil "~a/artifact/test-art.txt" host)
               :ensure-success t
               :want-string t))))))

(test artifact-link ()
  (with-fixture state ()
    (with-open-file (output
                     (ensure-directories-exist
                      (path:catfile
                       (object-store)
                       "artifacts/test-art.txt"))
                     :direction :output)
      (let ((link (artifact-link "test-art.txt")))
        (is (str:starts-with-p "/artifact/test-art.txt?"
                               link))
        (finishes
          (parse-integer
           (car (last (str:split "=" link)))))))))

(test hooks
  (with-fixture state ()
    (let ((count 0))
      (def-artifact-hook ('foo "bar")
        (incf count))
      (call-hooks "bar")
      (is (eql 1 count)))))
