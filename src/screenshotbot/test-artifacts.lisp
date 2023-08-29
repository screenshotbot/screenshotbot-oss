(defpackage :screenshotbot/test-artifacts
  (:use #:cl
        #:fiveam)
  (:import-from #:util/store
                #:*object-store*
                #:with-test-store
                #:object-store)
  (:import-from #:bknr.datastore
                #:*store*)
  (:import-from #:bknr.datastore
                #:store-directory)
  (:import-from #:util/testing
                #:with-local-acceptor
                #:with-fake-request)
  (:import-from #:screenshotbot/secret
                #:secret)
  (:import-from #:screenshotbot/artifacts
                #:def-artifact-hook
                #:call-hooks
                #:*artifact-hooks*
                #:*in-test-p*
                #:artifact-link)
  (:import-from #:screenshotbot/testing
                #:with-installation)
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
