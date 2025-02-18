;; Copyright 2018-Present Modern Interpreters Inc.
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/sdk/sdk
  (:nicknames :screenshotbot-sdk)
  (:use #:cl
        #:alexandria
        #:anaphora
        #:screenshotbot/sdk/flags
        #:screenshotbot/sdk/hostname)
  (:import-from #:dag
                #:add-commit
                #:commit
                #:merge-dag
                #:get-commit
                #:write-to-stream
                #:read-from-stream)
  (:import-from #:screenshotbot/sdk/bundle
                #:image-stream
                #:md5-sum
                #:list-images
                #:close-image
                #:image-name
                #:image-directory
                #:image-directory-with-diff-dir)
  (:import-from #:screenshotbot/sdk/git
                #:fetch-remote-branch
                #:null-repo
                #:git-root
                #:git-repo
                #:current-commit
                #:rev-parse
                #:read-graph
                #:cleanp
                #:repo-link
                #:merge-base)
  (:import-from #:util/request
                #:engine
                #:http-request)
  (:import-from #:util/misc
                #:?.
                #:or-setf)
  (:import-from #:screenshotbot/sdk/version-check
                #:*client-version*
                #:remote-supports-put-run
                #:remote-supports-basic-auth-p)
  (:import-from #:util/health-check
                #:def-health-check)
  (:import-from #:screenshotbot/api/model
                #:*api-version*
                #:encode-json)
  (:import-from #:util/json-mop
                #:ext-json-serializable-class
                #:json-mop-to-string)
  (:import-from #:screenshotbot/sdk/backoff
                #:maybe-retry-request
                #:backoff)
  (:import-from #:screenshotbot/sdk/api-context
                #:api-context)
  (:import-from #:screenshotbot/sdk/run-context
                #:flags-run-context)
  (:import-from #:util/threading
                #:with-extras)
  (:local-nicknames (#:flags #:screenshotbot/sdk/flags)
                    (#:dto #:screenshotbot/api/model)
                    (#:e #:screenshotbot/sdk/env)
                    (#:api-context #:screenshotbot/sdk/api-context)
                    (#:android   #:screenshotbot/sdk/android)
                    (#:run-context #:screenshotbot/sdk/run-context))
  (:export
   #:single-directory-run
   #:*request*
   #:*put*
   #:request
   #:put-file
   #:parse-org-defaults
   #:run-prepare-directory-toplevel
   #:absolute-pathname
   #:update-commit-graph
   #:validate-pull-request))

(in-package :screenshotbot/sdk/sdk)

(defmacro defopt (var &key params
                        default
                        boolean
                        required
                        (help "undocumented"))
  (declare (ignore required))
  (let ((params (or params
                    (unless boolean
                      `(list ,(str:replace-all "*" "" (string var)))))))
    `(list ',var ,default ,help :params ,params)))


(defclass model ()
  (type))

(defclass image (model)
  ((id :type string)
   (upload-url :type string)))

(define-condition api-error (error)
  ((message :initarg :message)))

(defmethod print-object ((e api-error) stream)
  (with-slots (message) e
   (format stream "#<API-ERROR ~a>" message)))

(defun ensure-api-success (result)
  (let ((indent "    "))
   (awhen (assoc-value result :error)
     (log:error "API error: ~a" it)
     (when-let ((stacktrace (assoc-value result :stacktrace)))
      (log:error "Server stack trace: ~%~a~a"
                 indent
                 (str:join (format nil "~%~a" indent)
                           (str:lines stacktrace))))
     (error 'api-error :message it)))
  (assoc-value result :response))


(defmethod %make-basic-auth (api-context)
  (list
   (api-context:key api-context)
   (api-context:secret api-context)))

(auto-restart:with-auto-restart (:attempt attempt)
  (defun %request (api-context
                   api &key (method :post)
                         parameters
                         content
                         (auto-retry t)
                         (backoff 2))
    ;; TODO: we're losing the response code here, we need to do
    ;; something with it.

    (when (and auto-retry
               (streamp content))
      (warn "auto-retry won't work for stream content"))

    (multiple-value-bind (stream response-code)
        (handler-bind ((error (lambda (e)
                                (warn "Retrying request because of error, is this a timeout? ~a" e)
                                (when auto-retry
                                  (maybe-retry-request
                                   408 ;; This is most likely the case this low down
                                   :attempt attempt
                                   :restart 'retry-%request
                                   :errorp nil
                                   :backoff backoff)))))
          (http-request
           (format-api-url api-context api)
           :method method
           :want-stream t
           :method method
           :basic-authorization (when (remote-supports-basic-auth-p api-context)
                                  (%make-basic-auth api-context))
           :additional-headers `(("X-client-version" . ,*client-version*)
                                 ("X-client-api-version" . ,*api-version*))
           :content content
           :external-format-out :utf-8
           :read-timeout 45
           :engine (api-context:engine api-context)
           :parameters (cond
                         ((remote-supports-basic-auth-p api-context)
                          parameters)
                         (t (list*
                             (cons "api-key" (api-context:key api-context))
                             (cons "api-secret-key" (api-context:secret api-context))
                             parameters)))))
      (when auto-retry
        (maybe-retry-request
         response-code
         :attempt attempt
         :restart 'retry-%request
         :backoff backoff))

      (with-open-stream (stream stream)
        (values
         (uiop:slurp-input-stream 'string stream)
         response-code)))))

(defmethod request ((api-context api-context:api-context)
                    api &key (method :post)
                          parameters
                          (decode-response t)
                          content)
  (log:debug "Making API request: ~S" api)
  (when (and (eql method :get) parameters)
    (error "Can't use :get with parameters"))
  (multiple-value-bind (json code)
      (%request api-context
                api :method method
                    :parameters parameters
                    :content (cond
                               ((or
                                 (eql :put method)
                                 (typep (class-of content)
                                        'ext-json-serializable-class))
                                (json-mop-to-string
                                 content))
                               ((and (eql method :post)
                                     content)
                                content)))
   (cond
     (decode-response
      (handler-case
          (let ((result (json:decode-json-from-string json)))
            (ensure-api-success result))
        (json:json-syntax-error (e)
          (error "Could not parse json:"
                 json))))
     (t
      (values json code)))))

(defun call-with-file-stream (non-file-stream fn)
  "See doc for with-file-stream"
  (handler-case
      (file-length non-file-stream)
    (type-error ()
      (error "Unimplemented non-file-stream: this is a bug, please ping support@screenshotbot.io")))
  (funcall fn non-file-stream))

(defmacro with-file-stream ((stream non-file-stream) &body body)
  "This actually does nothing: it just calls the body with stream bound to non-file-stream.

However: the intention is that we bind stream to a stream where we can
call file-length. Currently, it appears that the only streams we would
actually call put-file/upload-image with are file streams, so we just
don't implement this behavior for other streams. However, this
analysis was made later, so we're keeping this code here and if we get
a stram on which file-length doesn't work we raise a more parseable
error."
  `(call-with-file-stream
    ,non-file-stream
    (lambda (,stream) ,@body)))

(auto-restart:with-auto-restart (:attempt attempt)
  (defun put-file (api-context upload-url stream &key parameters)
    ;; In case we're retrying put-file, let's make sure we reset the
    ;; stream
    (log:debug "put file to: ~a" upload-url)
    (file-position stream 0)
    (with-file-stream (stream stream)
     (let ((file-length (file-length stream)))
       (log:debug "Got file length: ~a" file-length)
       (multiple-value-bind (result code)
         (http-request
          upload-url
          :method :put
          :parameters parameters
          ;; Basic auth for image puts will be supported from API
          ;; level 4, but in previous versions it should just be ignored.
          :basic-authorization (%make-basic-auth api-context)
          :content-type "application/octet-stream"
          :content-length file-length
          :engine (engine api-context)
          :content stream
          :want-string t ;; Required for the engine to be able to reuse connections
          :read-timeout 40)

         (log:debug "Got image upload response: ~s" result)

         (maybe-retry-request
          code
          :attempt attempt
          :restart 'retry-put-file)

         (unless (eql 200 code)
           (error "Failed to upload image: code ~a" code))
         result)))))

(defun build-screenshot-objects (images)
  (loop for im in images
        collect
        (let ((name (assoc-value im :name)))
          (make-instance 'dto:screenshot
                         :name name
                         :image-id (assoc-value im :id)))))

(defun safe-parse-int (str)
  (cond
    ((not str)
     nil)
    ((stringp str)
     (unless (str:emptyp str)
       (parse-integer str :junk-allowed t)))
    ((numberp str)
     str)
    (t
     (error "Not a type that can be convered to integer: ~s" str))))

(define-condition empty-run-error (error)
  ()
  (:report "No screenshots were detected in this this run. Perhaps you wanted to use the --recursive flag?"))

(auto-restart:with-auto-restart ()
 (defun make-run (api-context
                  screenshots &rest args
                  &key repo
                    channel
                    run-context
                    pull-request
                    branch
                    (branch-hash nil has-branch-hash-p)
                    (commit nil has-commit-p)
                    (merge-base nil has-merge-base-p)
                    github-repo ;; probably only used by replay T1708
                    periodic-job-p
                    create-github-issue
                    is-trunk)
   (loop for screenshot in screenshots
         ;; We expect it to be a dto:screenshot with name and image-id
         do (check-type screenshot dto:screenshot))
   (let ((extra-run-context-args nil))
     (flet ((push-extra-arg (key value)
              (setf extra-run-context-args
                    (list* key value extra-run-context-args))))
       (when has-commit-p
         #+nil ;; T1708
         (warn ":has-commit-p is still being used")
         (push-extra-arg :commit-hash commit))

       (when has-merge-base-p
         #+nil ;; T1708
         (warn ":has-merge-base-p is still being used")
         (push-extra-arg :merge-base merge-base))

       (when has-branch-hash-p
         (push-extra-arg :main-branch-hash branch-hash))

       (let ((run-context (or
                           run-context
                           ;; TODO: move out of make-run:
                           (apply #'make-instance 'run-context:flags-run-context
                                  :repo-url (or (?. repo-link repo)
                                                github-repo)
                                  :channel channel
                                  :pull-request-url pull-request
                                  :productionp is-trunk
                                  :create-github-issue-p create-github-issue
                                  :env (e:make-env-reader)
                                  extra-run-context-args))))
         (unless (or screenshots (run-context:shard-spec run-context))
           (error 'empty-run-error))
     
         ;;(log:info "screenshot records: ~s" screenshots)
         (let* ((run (make-instance 'dto:run
                                    :channel (run-context:channel run-context)
                                    :screenshots screenshots
                                    :metadata (run-context:run-context-metadata run-context)
                                    :main-branch branch
                                    :shard-spec (run-context:shard-spec run-context)
                                    :work-branch (run-context:work-branch run-context)
                                    :main-branch-hash (run-context:main-branch-hash run-context)
                                    :github-repo (run-context:repo-url run-context)
                                    :merge-base (run-context:merge-base run-context)
                                    :author (run-context:author run-context)
                                    :periodic-job-p periodic-job-p
                                    :build-url (run-context:build-url run-context)
                                    :compare-threshold (run-context:compare-threshold run-context)
                                    :batch (run-context:batch run-context)
                                    :pull-request (run-context:pull-request-url run-context)
                                    :commit-hash (run-context:commit-hash run-context)
                                    :override-commit-hash (run-context:override-commit-hash run-context)
                                    :create-github-issue-p (run-context:create-github-issue-p run-context)
                                    :cleanp (cleanp repo)
                                    :tags (run-context:tags run-context)
                                    :gitlab-merge-request-iid (safe-parse-int
                                                               (run-context:gitlab-merge-request-iid run-context))
                                    :phabricator-diff-id (safe-parse-int
                                                          (run-context:phabricator-diff-id run-context))
                                    :trunkp (run-context:productionp run-context))))
           (if (remote-supports-put-run api-context)
               (put-run api-context run)
               (put-run-via-old-api api-context run))))))))

(auto-restart:with-auto-restart ()
  (defmethod put-run ((api-context api-context) run)
    (let ((result (request api-context
                           "/api/run" :method :put
                                      :content run)))
      (let ((run-url (assoc-value result :url)))
        (cond
          (run-url
           (log:info "Created run: ~a" run-url))
          (t
           (log:info "Run wasn't created yet, probably because all shards weren't available"))))
      (make-instance 'dto:run
                     :id (assoc-value result :id)))))

(defun put-run-via-old-api (api-context run)
  (flet ((bool (x) (if x "true" "false")))
    (request
     api-context
     "/api/run"
     :parameters `(("channel" . ,(dto:run-channel run))
                   ("screenshot-records" . ,(json:encode-json-to-string
                                             (dto:run-screenshots run)))
                   ("branch" . ,(dto:main-branch run))
                   ("branch-hash" . ,(dto:main-branch-hash run))
                   ("github-repo" . ,(dto:run-repo run))
                   ("merge-base" . ,(dto:merge-base run))
                   ("periodic-job-p" . ,(bool (dto:periodic-job-p run)))
                   ("build-url" . ,(dto:build-url run))
                   ("pull-request" . ,(dto:pull-request-url run))
                   ("commit" . ,(dto:run-commit run))
                   ("override-commit-hash" . ,(dto:override-commit-hash run))
                   ("create-github-issue" . ,(bool (dto:should-create-github-issue-p run)))
                   ("is-clean" . ,(bool (dto:cleanp run)))
                   ("gitlab-merge-request-iid" .
                                               ,(dto:gitlab-merge-request-iid run))
                   ("phabricator-diff-id" . ,(dto:phabricator-diff-id run))
                   ("is-trunk" . ,(bool (dto:trunkp run)))))))


(defun $! (&rest args)
  (multiple-value-bind (out error res)
      (uiop:run-program args
                        :error-output :interactive
                        :output :interactive
                        :ignore-error-status t)
    (declare (ignore out error))
    (eql 0 res)))

(defclass basic-directory-run ()
  ((directory :initarg :directory)))

(defmethod make-directory-run (api-context dir &rest args)
  (log:debug "Reading images from ~a" dir)
  (let ((images
          (upload-image-directory api-context dir)))
    (log:debug "Creating run")
    (apply 'make-run
           api-context
           images
           args)))

(defun keyword-except-md5 (identifier)
  ;; See unit tests for this with the same name to understand why we
  ;; need this.
  ;;
  ;; The implementation is bug prone, but I know for a fact we don't
  ;; have 32 character long identifiers in the json :/
  (cond
   ((eql 32 (length identifier))
    (string identifier))
   (t
    (json:camel-case-to-lisp identifier))))

(define-condition not-recent-file-warning (warning)
  ((file :initarg :file))
  (:report
   (lambda (self output)
     (with-slots (file) self
      (format output "The file ~a is older than 48hrs, and might be a stale screenshot from a previous build"
              file)))))

(defun warn-if-not-recent-file (stream)
  ;; Heads up: file-write-date can return NIL, see T1632. Unable to
  ;; repro in tests though.
  (when-let ((write-date (file-write-date stream)))
    (when (< write-date
             (- (get-universal-time) (* 48 3600)))
      (with-extras (("file-ts" write-date))
        (warn 'not-recent-file-warning :file (pathname stream))))))

(defmethod find-existing-images (api-context
                                 hashes)
  (let ((json:*json-identifier-name-to-lisp* #'keyword-except-md5))
    (let ((result
            (request
             api-context
             "/api/screenshot"
             :parameters `(("hash-list"
                            . ,(json:encode-json-to-string hashes))))))
      ;; The response looks like:
      ;;(:|82142ae81caba45bb76aa21fb6acf16d| (:TYPE . "image") (:ID . "6748c0b00572379fe0c9d0d0") (:UPLOAD-URL . "http://NIL/api/image/blob?oid=6748c0b00572379fe0c9d0d0"))
      (loop for (key . body) in result
            collect
            (make-instance 'dto:image-upload-response
                           :md5sum (str:downcase (string key))
                           :image-id (assoc-value body :id)
                           :upload-url (assoc-value body :upload-url))))))

(defmethod upload-image-directory (api-context bundle)
  (let ((images (list-images bundle)))
    (let* ((hashes (mapcar 'md5-sum images))
           (image-upload-responses
             (find-existing-images api-context hashes)))
      (let ((md5-to-response (make-hash-table :test #'equal)))
        ;; Create a map from from the md5sum to the the response
        ;; object for quick lookup.
        (loop for image-upload-response in image-upload-responses
              do (setf (gethash (str:downcase (dto:image-md5sum image-upload-response))
                                md5-to-response)
                       image-upload-response))
        (loop for im in images
              collect
              (%upload-single-image api-context im md5-to-response))))))

(defun %upload-single-image (api-context im md5-to-response)
  "This is a stateful function. In particular, as and when an upload URL
is used up, we update md5-to-response to remove that upload URL."
  (let* ((response (gethash (str:downcase (md5-sum im))
                            md5-to-response))
         (key (image-name im))
         (upload-url (dto:image-upload-url response)))
    (with-open-stream (stream (image-stream im))
      (warn-if-not-recent-file stream)
      (unwind-protect
           (progn
             (when upload-url
               (log:info "Uploading image for `~a`" key)
               (put-file api-context upload-url stream))

             ;; This Upload-URL is used up, if there's another image
             ;; with the same hash, we shouldn't upload it again
             (setf (dto:image-upload-url response) nil)

             (make-instance 'dto:screenshot
                            :image-id (dto:image-id response)
                            :name key))
        (close-image im)))))

(defun make-bundle (&key (metadata flags:*metadata*)
                      (directory flags:*directory*)
                      (recursivep flags:*recursive*))
  (cond
    (metadata
     (log:info "Looks like an Android run")
     (android:make-image-bundle :metadata metadata))
    ((not (str:emptyp directory))
     (unless (path:-d directory)
       (error "Not a directory: ~a" directory))
     (make-instance 'image-directory
                    :directory directory
                    :file-types (str:split "," flags:*image-file-types*)
                    :recursivep recursivep))
    (t
     (error "Unknown run type, maybe you missed --directory?"))))

(defun guess-master-branch (repo)
  (flet ((check (x)
           (rev-parse repo x)))
    (cond
      ((check "main")
       "main")
      ((check "master")
       "master")
      (t
       (error "Could not guess the main branch, please use --main-branch argument")))))

(define-condition invalid-pull-request (condition)
  ())

(defun validate-pull-request ()
  "One of our customers is using an incorrect --pull-request arg. The
incorrect arg breaks some logic, and additionally is not required
since we can determine the pull-request from the environment. We can
do a quick sanity check, and recover with a warning if the
pull-request looks incorrect."
  (when flags:*pull-request*
   (flet ((validp (url)
            (str:starts-with-p "https://" url)))
     (unless (validp flags:*pull-request*)
       (signal 'invalid-pull-request)
       (log:warn "The --pull-request argument you provided was invalid: `~a`. We're ignoring this.~%"
             flags:*pull-request*)
       (setf flags:*pull-request* nil)))))

(defun parse-environment ()
  (let* ((env (e:make-env-reader))
         ;; TODO: we should use run-ctx for all our computations, and
         ;; eventually remove the setf-ing of flags. See T795
         (run-ctx (make-instance 'flags-run-context
                                 :env env)))
    (when flags:*branch*
      (error "--branch is no longer supported, please use --main-branch instead"))

    (validate-pull-request)

    ;; TODO: we shouldn't refer to the flags from this point onwards,
    ;; instead just using the run-context.

    (setf flags:*build-url*
          (run-context:build-url run-ctx))

    (setf flags:*repo-url*
          (run-context:repo-url run-ctx))

    (setf flags:*work-branch*
          (run-context:work-branch run-ctx))

    (setf flags:*pull-request*
          (run-context:pull-request-url run-ctx))

    (setf flags:*channel*
          (run-context:channel run-ctx))

    (setf
     flags:*override-commit-hash*
     (run-context:override-commit-hash run-ctx))

    (setf
     flags:*main-branch*
     (run-context:main-branch run-ctx))))

(defun link-to-github-pull-request (repo-url pull-id)
  (let ((key (cond
               ((str:containsp "bitbucket" repo-url)
                "pull-requests")
               (t
                "pulls"))))
   (format nil "~a/~a/~a"
           repo-url
           key
           pull-id)))


(defun parse-org-defaults ()
  (parse-environment))

(defun recursive-directories (directory)
  (or
   (loop for d in (fad:list-directory directory)
         if (and (not (str:starts-with-p "." (car (last (pathname-directory d)))))
                 (path:-d d))
           appending (recursive-directories d))
   (list directory)))

(defun get-relative-path (dir parent)
  (let ((dir-parts (pathname-directory dir))
        (parent-parts (pathname-directory parent)))
    (log:debug "got parts: ~s ~s" parent-parts dir-parts)
    (assert (equal parent-parts
                   (subseq dir-parts 0 (length parent-parts))))
    (let ((res (make-pathname
                :directory
                `(:relative ,@(subseq dir-parts (length parent-parts)))
                :defaults #P"./")))
      (log:debug "Relative path parts: ~S" (pathname-directory res))
      (log:debug "Relative path is: ~S" res)
      res)))

(defmethod update-commit-graph (api-context repo branch)
  (fetch-remote-branch repo branch)
  (log:info "Updating commit graph")
  (let* ((dag (read-graph repo))
         (json (with-output-to-string (s)
                 (dag:write-to-stream dag s))))
    (request
     api-context
     "/api/commit-graph"
     :method :post
     :parameters (list
                  (cons "repo-url" (repo-link repo))
                  (cons "branch" branch)
                  (cons "graph-json" json)))))

(defmethod update-commit-graph (api-context (repo null-repo) branch)
  (log:info "Not updating the commit graph, since there's no repo"))

(defun git-repo ()
  (let ((root (ignore-errors (git-root))))
    (cond
      (root
       (make-instance 'git-repo
                      :link flags:*repo-url*))
      (t
       (log:warn "This is not running inside a Git repo. Please contact support@screenshotbot.io for advice, since the behavior in this case can be very different.")
       (make-instance 'null-repo
                      :link flags:*repo-url*)))))

(defun single-directory-run (api-context directory &key channel)
  (let ((repo (git-repo))
        (branch flags:*main-branch*))
    (when (and flags:*production*
               (> flags:*commit-limit* 0))
      (update-commit-graph api-context repo branch))
    (log:info "Uploading images from: ~a" directory)
    (make-directory-run api-context directory
                        :channel channel
                        :pull-request flags:*pull-request*
                        :create-github-issue flags:*create-github-issue*
                        :repo repo
                        :is-trunk flags:*production*
                        :branch branch)))

(defun chdir-for-bin (path)
  (uiop:chdir path)
  (setf *default-pathname-defaults* (pathname path)))


(defun absolute-pathname (p)
  (fad:canonical-pathname (path:catdir (uiop:getcwd) p)))

(defun run-prepare-directory-toplevel (api-context)
  (let ((directory (make-bundle)))
    (single-directory-run api-context directory :channel flags:*channel*)))

(def-health-check verify-https-works ()
  (util/request:http-request "https://screenshotbot.io/api/version"
                             :ensure-success t))
