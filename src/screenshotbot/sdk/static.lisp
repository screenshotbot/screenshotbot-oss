(defpackage :screenshotbot/sdk/static
  (:use #:cl)
  (:import-from #:screenshotbot/replay/browser-config
                #:dimensions
                #:browser-config)
  (:local-nicknames (#:a #:alexandria)
                    (#:sdk #:screenshotbot/sdk/sdk)
                    (#:flags #:screenshotbot/sdk/flags)
                    (#:replay #:screenshotbot/replay/core))
  (:export
   #:record-static-website))
(in-package :screenshotbot/sdk/static)


(defun md5-file (file)
  (ironclad:byte-array-to-hex-string (ironclad:digest-file :sha256 file)))

(defun upload-blob (file)
  "Upload the blob, without checking if it has been previously uploaded"
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
  "Check if the given list of files have already been uploaded in the paste"
  (let ((input (loop for file in files
                     collect `((:file . ,(namestring file))
                               (:hash . ,(md5-file file))
                               (:type . ,(pathname-type file))))))
    (sdk:request "/api/blob/check"
                 :method :post
                 :parameters `(("query" . ,(json:encode-json-to-string input))))))

(defun upload-multiple-files (files)
  "Upload multiple blobs efficiently, checking to make sure we only
upload blobs that haven't been uploaded before."
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

(defun upload-snapshot-assets (snapshot)
  "Upload all the assets in the snapshot"
  (upload-multiple-files
   (append
    (loop for asset in (replay:assets snapshot)
          collect
          (replay:snapshot-asset-file snapshot asset)))))

(defun browser-configs ()
  "Currently we're not providing a way to modify the browser
  config. We're hardcoding this to a value that we can initially work
  with."
  (list
   (make-instance 'browser-config
                  :type "chrome"
                  :dimensions (make-instance 'dimensions
                                            :width 1280
                                            :height 800)
                  :name "Google Chrome Desktop")
   (make-instance 'browser-config
                  :type "chrome"
                  :name "Nexus 5 (emulated)"
                  ;; yes, this inconsistency is intentional for now.
                  :mobile-emulation "Nexus 6P")))

(defun schedule-snapshot (snapshot)
  "Schedule a Replay build with the given snapshot"
  (setf (replay:tmpdir snapshot) nil) ;; hack for json encoding
  (let ((request (make-instance 'replay:snapshot-request
                                :snapshot snapshot
                                :channel-name flags:*channel*
                                :browser-configs (browser-configs))))
   (json:with-decoder-simple-clos-semantics
     (uiop:with-temporary-file (:pathname p)
       (cl-store:store request p)
       (let ((uri (quri:make-uri
                   :query `(("api-key" . ,flags:*api-key*)
                            ("api-secret-key" . ,flags:*api-secret*))
                   :defaults
                   (quri:uri (format nil "~a/api/replay/schedule" flags:*hostname*)))))
         (dex:post uri
                   :content p))))))

(defun record-static-website (location)
  (assert (path:-d location))
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

                 (upload-snapshot-assets snapshot)
                 (schedule-snapshot snapshot)
                 (log:info "Screenshot job queued"))
            (hunchentoot:stop acceptor))))
    (retry-record-static-website ()
      (record-static-website location))))
