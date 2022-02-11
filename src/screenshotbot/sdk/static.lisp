(defpackage :screenshotbot/sdk/static
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria)
                    (#:sdk #:screenshotbot/sdk/sdk)
                    (#:flags #:screenshotbot/sdk/flags)
                    (#:replay #:screenshotbot/replay/core))
  (:export
   #:record-static-website))
(in-package :screenshotbot/sdk/static)


(defun md5-file (file)
  (ironclad:byte-array-to-hex-string (md5:md5sum-file file)))

(defun upload-blob (file)
  (log:info "Uploading file: ~a" file)
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


(defun blob-check (files)
  (let ((input (loop for file in files
                     collect `((:file . ,(namestring file))
                               (:hash . ,(md5-file file))
                               (:type . ,(pathname-type file))))))
    (sdk:request "/api/blob/check"
                 :method :post
                 :parameters `(("query" . ,(json:encode-json-to-string input))))))

(defun upload-multiple-files (files)
  (restart-case
      (let ((results (blob-check files)))
        (loop for result in results
              for existsp = (a:assoc-value result :exists)
              for file = (a:assoc-value result :file)
              if (not existsp)
                do
                   (upload-blob file)))
    (retry-upload-multiple-files ()
      (upload-multiple-files files))))

#+nil
(let ((flags:*hostname* "https://staging.screenshotbot.io")
      (flags:*api-key* *key*)
      (flags:*api-secret* *secret*))
  (upload-blob "~/builds/web/update-ip.lisp"))

#+nil
(let ((flags:*hostname* "https://staging.screenshotbot.io")
      (flags:*api-key* *key*)
      (flags:*api-secret* *secret*))
  (upload-multiple-files
   (list
    "~/builds/web/update-ip.lisp"
    "~/builds/web/Makefile")))

#+nil
(let ((flags:*hostname* "https://staging.screenshotbot.io")
      (flags:*api-key* *key*)
      (flags:*api-secret* *secret*))
  (record-static-website "~/builds/gatsby-example/public/"))

(defun record-static-website (location)
  (restart-case
      (tmpdir:with-tmpdir (tmpdir)
        (let* ((port (util/random-port:random-port))
               (acceptor (make-instance 'hunchentoot:acceptor
                                        :port port
                                        :document-root location))
               (snapshot (make-instance 'replay:snapshot
                                        :tmpdir tmpdir)))
          (unwind-protect
               (progn
                 (hunchentoot:start acceptor)
                 (replay:load-url-into snapshot (format nil "http://localhost:~a/index.html" port) tmpdir)
                 (error "unimplemented ~a" snapshot))
            (hunchentoot:stop acceptor))))
    (retry-record-static-website ()
      (record-static-website location))))
