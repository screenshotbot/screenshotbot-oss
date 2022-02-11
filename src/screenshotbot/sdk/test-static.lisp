(defpackage :screenshotbot/sdk/test-static
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/server
                #:acceptor)
  (:import-from #:screenshotbot/sdk/static
                #:upload-blob)
  (:local-nicknames (#:a #:alexandria)
                    (#:flags :screenshotbot/sdk/flags)))
(in-package :screenshotbot/sdk/test-static)

(util/fiveam:def-suite)

(defvar *hex* nil)

(defun cleanup ()
  (setf *hex* nil))

(def-fixture state ()
  (cleanup)
  (let* ((port (util/random-port:random-port))
         (acceptor (make-instance 'hunchentoot:easy-acceptor
                                  :name 'test-acceptor
                                  :port port)))
    (unwind-protect
         (progn
           (hunchentoot:start acceptor)
           (&body))
      (hunchentoot:stop acceptor)
      (cleanup))))

(hunchentoot:define-easy-handler (fake-blob-upload :uri "/api/blob/upload"
                                                   :acceptor-names '(test-acceptor))
    (hash type api-key api-secret-key)
  (screenshotbot/api/image:with-raw-post-data-as-tmp-file (p)
    (setf *hex* (md5:md5sum-file p))))

(test blob-upload
  (with-fixture state ()
   (uiop:with-temporary-file (:pathname p :stream out)
     (write "zoidberg" :stream out)
     (finish-output out)
     (let ((flags:*hostname* (format nil "http://localhost:~a" port)))
       (let ((md5 (md5:md5sum-file p)))
         (upload-blob p)
         (is (equalp *hex* md5)))))))
