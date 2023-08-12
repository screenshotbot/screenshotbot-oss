(defpackage :screenshotbot/sdk/static
  (:use #:cl)
  (:import-from #:screenshotbot/replay/browser-config
                #:dimensions
                #:browser-config)
  (:import-from #:screenshotbot/replay/core
                #:*cache*
                #:context)
  (:import-from #:screenshotbot/sdk/sdk
                #:guess-master-branch
                #:request
                #:ensure-api-success)
  (:import-from #:util/digests
                #:sha256-file)
  (:import-from #:util/lru-cache
                #:lru-cache)
  (:import-from #:screenshotbot/sdk/common-flags
                #:*sdk-flags*)
  (:import-from #:easy-macros
                #:def-easy-macro)
  (:import-from #:util/misc
                #:with-global-binding)
  (:import-from #:file-lock
                #:release-file-lock
                #:file-lock)
  (:import-from #:alexandria
                #:remove-from-plist
                #:assoc-value)
  (:import-from #:util/request
                #:http-request)
  (:import-from #:clingon
                #:make-option)
  (:import-from #:clingon.command
                #:getopt)
  (:import-from #:screenshotbot/sdk/cli-common
                #:common-run-options
                #:*root-commands*
                #:with-clingon-api-context)
  (:local-nicknames (#:a #:alexandria)
                    (#:sdk #:screenshotbot/sdk/sdk)
                    (#:flags #:screenshotbot/sdk/flags)
                    (#:git #:screenshotbot/sdk/git)
                    (#:e #:screenshotbot/sdk/env)
                    (#:replay #:screenshotbot/replay/core)
                    (#:api-context #:screenshotbot/sdk/api-context))
  (:export
   #:record-static-website))
(in-package :screenshotbot/sdk/static)


(defun md5-file (file)
  (ironclad:byte-array-to-hex-string (sha256-file  file)))

(defun upload-blob (api-context file)
  "Upload the blob, without checking if it has been previously uploaded"
  (log:info "Uploading file: ~a" file)
  (with-open-file (stream file :direction  :input
                               :element-type '(unsigned-byte 8))
    (let ((uri (quri:make-uri
                  :query `(("hash" . ,(md5-file file))
                           ("type" . ,(pathname-type file)))
                  :defaults
                  (quri:uri (format nil "~a/api/blob/upload" (api-context:hostname api-context))))))
      (sdk:put-file api-context
                    (format nil "~a" uri)
                    stream))))


(defun blob-check (api-context files)
  "Check if the given list of files have already been uploaded in the paste"
  (let ((input (loop for file in files
                     collect `((:file . ,(namestring file))
                               (:hash . ,(md5-file file))
                               (:type . ,(pathname-type file))))))
    (sdk:request api-context
                 "/api/blob/check"
                 :method :post
                 :parameters `(("query" . ,(json:encode-json-to-string input))))))

(defun upload-multiple-files (api-context files)
  "Upload multiple blobs efficiently, checking to make sure we only
upload blobs that haven't been uploaded before."
  (let ((files (remove-if-not #'uiop:file-exists-p files)))
    (let ((results (blob-check api-context files)))
      (loop for result in results
            for existsp = (a:assoc-value result :exists)
            for file = (a:assoc-value result :file)
            if (not existsp)
              do
                 (upload-blob api-context file)))))


(defun upload-snapshot-assets (api-context snapshot)
  "Upload all the assets in the snapshot"
  (upload-multiple-files
   api-context
   (append
    (loop for asset in (replay:assets snapshot)
          collect
          (replay:snapshot-asset-file snapshot asset)))))

(defun parse-config-from-file (file)
  (with-open-file (stream (pathname file))
   (let ((configs (json:decode-json stream)))
     (loop for config in configs
           collect
           (flet ((dim ()
                    (a:when-let ((dim (assoc-value config :dimensions)))
                      (str:split "x" dim))))
             (make-instance 'browser-config
                            :type (assoc-value config :type)
                            :name (assoc-value config :name)
                            :dimensions (when (dim)
                                          (make-instance 'dimensions
                                                         :width (parse-integer (first (dim)))
                                                         :height (parse-integer (second (dim)))))
                            :mobile-emulation (assoc-value config :mobile-emulation)))))))

(defun read-browser-configs (file)
  "Currently we're not providing a way to modify the browser
  config. We're hardcooding this to a value that we can initially work
  with."
  (cond
    (file
     (parse-config-from-file file))
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

(defun get-sdk-flags ()
  (loop for key being the hash-keys of *sdk-flags*
        using (hash-value sym)
        collect (cons key (symbol-value sym))))

(defun schedule-snapshot (api-context snapshot &key channel
                                                 repo-url
                                                 browser-configs
                                                 main-branch
                                                 pull-request
                                                 production)
  "Schedule a Replay build with the given snapshot"
  (declare (ignore production)) ;; TODO
  (setf (replay:tmpdir snapshot) nil) ;; hack for json encoding
  (let* ((repo (make-instance 'git:git-repo :link repo-url))
         (branch-hash (ignore-errors (git:rev-parse repo main-branch)))
         (commit (ignore-errors (git:current-commit repo))))
   (let ((request (make-instance 'replay:snapshot-request
                                 :snapshot snapshot
                                 :channel-name channel
                                 :pull-request pull-request
                                 :main-branch main-branch
                                 :repo-url repo-url
                                 :browser-configs (read-browser-configs browser-configs)
                                 :sdk-flags (get-sdk-flags)
                                 :commit commit
                                 :branch-hash branch-hash
                                 :merge-base
                                 (git:merge-base repo branch-hash commit))))
     (uiop:with-temporary-file (:pathname p)
       (cl-store:store request p)
       (request
        api-context
        "/api/replay/schedule"
        :method :post
        :content p)))))

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
  (let ((path #+mswindows
              (path:catdir (pathname (uiop:getenv "APPDATA"))
                           #P"screenshotbot/cache/replay/")
              #-mswindows
              (pathname "~/.cache/screenshotbot/replay/")))
    (ensure-directories-exist path)
    (make-instance 'lru-cache
                   :dir path)))

(def-easy-macro with-cache-dir (&fn fn)
  (with-global-binding ((*cache* (ensure-cache-dir)))
    (let ((lock
            (make-instance 'file-lock
                           :file (path:catfile (util/lru-cache:dir *cache*)
                                               "lock"))))
      (unwind-protect
           (fn)
        (release-file-lock lock)))))

(auto-restart:with-auto-restart ()
  (defun record-static-website (api-context location
                                &rest args
                                &key production
                                  repo-url
                                  main-branch
                                  assets-root
                                  &allow-other-keys)
    (assert (path:-d location))
    (let ((git-repo (make-instance 'git:git-repo :link repo-url)))
      (when production
        (sdk:update-commit-graph
         api-context
         git-repo
         main-branch))
      (let ((schedule-args
              (remove-from-plist args
                                 :main-branch :assets-root))
            (main-branch (or main-branch
                             (guess-master-branch git-repo))))
        (with-cache-dir ()
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
                               (when assets-root
                                 (format nil "~a~a"
                                         assets-root
                                         index.html))))

                     (upload-snapshot-assets api-context snapshot)
                     (let* ((result (apply #'schedule-snapshot api-context snapshot
                                           :main-branch main-branch
                                           schedule-args))
                            (logs (a:assoc-value result :logs)))
                       (log:info "Screenshot job queued: ~a" logs)))
                (hunchentoot:stop acceptor)
                #+mswindows
                (progn
                  (log:info "[windows-only] Waiting 2s before cleanup")
                  (sleep 2))))))))
))

(defun static-website/command ()
  (clingon:make-command
   :name "static-website"
   :options (list*
             (make-option
              :string
              :long-name "assets-root"
              :description "The root for all image/CSS assets. If not provided it defaults to the directory"
              :key :assets-root)
             (make-option
              :string
              :long-name "directory"
              :required t
              :description "The directory that we scan for all HTML files."
              :key :directory)
             (make-option
              :string
              :long-name "browser-configs"
              :description "A JSON file that specifies which browsers to run this on."
              :key :browser-configs)
             (common-run-options))
   :description "Upload a directory of HTML assets with images/CSS and have Screenshotbot render them in browsers of your choice"
   :handler (lambda (cmd)
              (log:config :debug)
              (with-clingon-api-context (api-context cmd)
                (let ((env (e:make-env-reader)))
                  (apply #'record-static-website
                         api-context (getopt cmd :directory)
                         :pull-request (or (getopt cmd :pull-request)
                                           (e:pull-request-url env))
                         :repo-url (or (getopt cmd :repo-url)
                                       (e:repo-url env))
                         :browser-configs (getopt cmd :browser-configs)
                         :main-branch (getopt cmd :main-branch)
                         (loop for key  in `(:assets-root :production
                                             :channel)
                               append (list key (getopt cmd key)))))))))

(setf (assoc-value *root-commands* :static-website)
      #'static-website/command)
