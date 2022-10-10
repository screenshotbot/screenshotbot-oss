(defpackage :screenshotbot/sdk/static
  (:use #:cl)
  (:import-from #:screenshotbot/replay/browser-config
                #:dimensions
                #:browser-config)
  (:import-from #:screenshotbot/replay/core
                #:*cache*
                #:context)
  (:import-from #:screenshotbot/sdk/sdk
                #:ensure-api-success)
  (:import-from #:util/digests
                #:sha256-file)
  (:local-nicknames (#:a #:alexandria)
                    (#:sdk #:screenshotbot/sdk/sdk)
                    (#:flags #:screenshotbot/sdk/flags)
                    (#:git #:screenshotbot/sdk/git)
                    (#:replay #:screenshotbot/replay/core))
  (:export
   #:record-static-website))
(in-package :screenshotbot/sdk/static)


(defun md5-file (file)
  (ironclad:byte-array-to-hex-string (sha256-file  file)))

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
     (sdk:put-file (format nil "~a" uri)
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
      (let ((files (remove-if-not #'uiop:file-exists-p files)))
       (let ((results (blob-check files)))
         (loop for result in results
               for existsp = (a:assoc-value result :exists)
               for file = (a:assoc-value result :file)
               if (not existsp)
                 do
                    (upload-blob file))))
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
  (cond
    (flags:*browser-configs*
     (let ((config (nyaml:parse (pathname flags:*browser-configs*))))
       (loop for name being the hash-keys of config
             using (hash-value value)
             collect
             (flet ((dim ()
                      (a:when-let ((dim (gethash "dimensions" value)))
                       (str:split "x" dim))))
               (make-instance 'browser-config
                               :type (gethash "type" value)
                               :name name
                               :dimensions (when (dim)
                                             (make-instance 'dimensions
                                                             :width (parse-integer (first (dim)))
                                                             :height (parse-integer (second (dim)))))
                               :mobile-emulation (gethash "mobile-emulation" value))))))
    (t
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
                      :mobile-emulation "Nexus 6P")))))

(defun schedule-snapshot (snapshot)
  "Schedule a Replay build with the given snapshot"
  (setf (replay:tmpdir snapshot) nil) ;; hack for json encoding
  (let* ((repo (make-instance 'git:git-repo :link flags:*repo-url*))
         (branch-hash (ignore-errors (git:rev-parse repo flags:*main-branch*)))
         (commit (ignore-errors (git:current-commit repo))))
   (let ((request (make-instance 'replay:snapshot-request
                                 :snapshot snapshot
                                 :channel-name flags:*channel*
                                 :pull-request flags:*pull-request*
                                 :main-branch flags:*main-branch*
                                 :repo-url flags:*repo-url*
                                 :browser-configs (browser-configs)
                                 :commit commit
                                 :branch-hash branch-hash
                                 :merge-base (ignore-errors (git:merge-base repo branch-hash commit)))))
     (uiop:with-temporary-file (:pathname p)
       (cl-store:store request p)
       (let ((uri (quri:make-uri
                   :query `(("api-key" . ,flags:*api-key*)
                            ("api-secret-key" . ,flags:*api-secret*))
                   :defaults
                   (quri:uri (format nil "~a/api/replay/schedule" flags:*hostname*)))))
         (ensure-api-success
          (json:decode-json-from-string
           (dex:post uri
                     :content p
                     :force-string t))))))))

(defun find-all-index.htmls (dir)
  (declare (optimize (debug 3) (speed 0)))
  (labels ((walk (subdir prefix)
             (loop for item in (remove-if #'null (fad:list-directory subdir))
                   if (and
                       (string-equal "html" (pathname-type item)))
                     collect (format nil "~a/~a" prefix (path:basename item))
                   if (path:-d item)
                     appending (progn
                                 (assert item)
                                 (let ((prefix (format nil "~a/~a" prefix
                                                   (car (last (pathname-directory item))))))
                                  (walk item prefix))))))
    (walk dir "")))

(defun ensure-cache-dir ()
  (unless *cache*
    (let ((path #+mswindows
                (path:catdir (pathname (uiop:getenv "APPDATA"))
                             #P"screenshotbot/cache/replay/")
                #-mswindows
                (pathname "~/.cache/screenshotbot/replay/")))
      (ensure-directories-exist path)

     (setf *cache*
           (make-instance 'lru-cache
                           :dir path)))))

(defun record-static-website (location)
  (assert (path:-d location))
  (ensure-cache-dir)
  (when flags:*production*
   (sdk:update-commit-graph (make-instance 'git:git-repo :link flags:*repo-url*)
                            flags:*main-branch*))
  (restart-case
      (tmpdir:with-tmpdir (tmpdir)
        (let* ((context (make-instance 'context))
               (port (util/random-port:random-port))
               (acceptor (make-instance 'hunchentoot:acceptor
                                        :port port
                                        :document-root location))
               (snapshot (make-instance 'replay:snapshot
                                        :tmpdir tmpdir)))
          (unwind-protect
               (progn
                 (hunchentoot:start acceptor)
                 (loop for index.html in (find-all-index.htmls location)
                       do
                          (replay:load-url-into
                           context
                           snapshot
                           (format nil "http://localhost:~a~a" port index.html)
                           tmpdir
                           :actual-url
                           (when flags:*static-website-assets-root*
                             (format nil "~a~a"
                                     flags:*static-website-assets-root*
                                     index.html))))

                 (upload-snapshot-assets snapshot)
                 (let* ((result (schedule-snapshot snapshot))
                        (logs (a:assoc-value result :logs)))
                   (log:info "Screenshot job queued: ~a" logs)))
            (hunchentoot:stop acceptor)
            #+mswindows
            (progn
              (log:info "[windows-only] Waiting 2s before cleanup")
              (sleep 2)))))
    (retry-record-static-website ()
      (record-static-website location))))
