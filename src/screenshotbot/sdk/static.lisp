(defpackage :screenshotbot/sdk/static
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria)
                    (#:sdk #:screenshotbot/sdk/sdk)
                    (#:flags #:screenshotbot/sdk/flags)))
(in-package :screenshotbot/sdk/static)


(defun md5-file (file)
  (ironclad:byte-array-to-hex-string (md5:md5sum-file file)))

(defun upload-blob (file)
  (with-open-file (stream file :direction  :input
                               :element-type '(unsigned-byte 8))
    (let ((uri (quri:make-uri
                  :query `(("hash" . ,(md5-file file))
                           ("type" . ,(pathname-type file))
                           ("api-key" . ,flags:*api-key*)
                           ("api-secret-key" . ,flags:*api-secret*))
                  :defaults
                  (quri:uri (format nil "~a/api/blob/upload" flags:*hostname*)))))
     (sdk:put-file uri
                   stream))))

#+nil
(let ((flags:*hostname* "https://staging.screenshotbot.io")
      (flags:*api-key* *key*)
      (flags:*api-secret* *secret*))
 (upload-blob "~/builds/web/update-ip.lisp"))
