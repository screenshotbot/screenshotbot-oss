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
                #:*in-test-p*
                #:ensure-private-ip
                #:artifact-link)
  (:local-nicknames (#:a #:alexandria)))
(in-package :screenshotbot/test-artifacts)


(util/fiveam:def-suite)

(def-fixture state ()
  (tmpdir:with-tmpdir (dir)
    (assert (not (boundp '*object-store*)))
    (let ((old-secret (secret :artifact-upload-key)))
      (unwind-protect
           (progn
             (setf *object-store* (namestring dir))
             (setf *in-test-p* t)
             (setf (secret :artifact-upload-key) "foobar")
             (with-local-acceptor (host) ('screenshotbot/server:acceptor)
               (&body)))
        (setf (secret :artifact-upload-key) old-secret)
        (setf *in-test-p* nil)
        (makunbound '*object-store*)))))

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

(test upload-asset ()
  (with-fixture state ()
    (with-open-file (output
                     (ensure-directories-exist
                      (path:catfile
                       (object-store)
                       "artifacts/test-art.txt"))
                     :direction :output)
      (write-string "hello2" output)
      (finish-output output)

      (is
       (equal
        "OK"
        (util/request:http-request
         (format nil "~a/intern/artifact/upload?name=test-art.txt&hash=~a&upload-key=foobar"
                 host
                 (ironclad:byte-array-to-hex-string
                  (md5:md5sum-string "zoidberg")))
         :method :put
         :ensure-success t
         :want-string t
         :content "zoidberg")))

      (is
       (equal "zoidberg"
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
