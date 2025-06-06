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
                #:invalid-pull-request
                #:flags-run-context)
  (:import-from #:util/threading
                #:with-extras)
  (:import-from #:screenshotbot/sdk/request
                #:request
                #:%make-basic-auth)
  (:import-from #:screenshotbot/sdk/commit-graph
                #:commit-graph-updater
                #:update-commit-graph)
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
   #:validate-pull-request
   #:upload-image-directory
   #:make-run))

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
                    (before #'identity)
                    branch
                    (branch-hash nil has-branch-hash-p)
                    (commit nil has-commit-p)
                    (merge-base nil has-merge-base-p)
                    github-repo ;; probably only used by replay T1708
                    periodic-job-p
                    is-trunk)
   "BEFORE is a callback that is called before creating the run. It is
called with the RUN-CONTEXT which is either the one passed or the
run-context that was created here."

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
                                  :productionp is-trunk
                                  :main-branch branch
                                  :env (e:make-env-reader)
                                  extra-run-context-args))))
         (funcall before run-context)
         (unless (or screenshots (run-context:shard-spec run-context))
           (error 'empty-run-error))

         (unless (str:emptyp branch)
           (unless (equal branch (run-context:main-branch run-context))
             (warn "branch does not match run-context: ~a vs ~a" branch (run-context:main-branch run-context))))
     
         ;;(log:info "screenshot records: ~s" screenshots)
         (put-run-with-run-context
          api-context
          run-context
          screenshots
          :cleanp (cleanp repo)
          :periodic-job-p periodic-job-p))))))

(defmethod put-run-with-run-context (api-context run-context screenshots
                                     &key
                                       ;; periodic-job is only set
                                       ;; from the server side at the
                                       ;; moment, so it's not part of the run-context
                                       periodic-job-p
                                       ;; TODO: make cleanp come from the run-context instead.
                                       cleanp)
  (let* ((run (make-instance 'dto:run
                             :channel (run-context:channel run-context)
                             :screenshots screenshots
                             :metadata (run-context:run-context-metadata run-context)
                             :main-branch (run-context:main-branch run-context)
                             :shard-spec (run-context:shard-spec run-context)
                             :work-branch (run-context:work-branch run-context)
                             :release-branch-p (run-context:work-branch-is-release-branch-p run-context)
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
                             :cleanp cleanp
                             :tags (run-context:tags run-context)
                             :gitlab-merge-request-iid (safe-parse-int
                                                        (run-context:gitlab-merge-request-iid run-context))
                             :phabricator-diff-id (safe-parse-int
                                                   (run-context:phabricator-diff-id run-context))
                             :trunkp (run-context:productionp run-context))))
    (if (remote-supports-put-run api-context)
        (put-run api-context run)
        (put-run-via-old-api api-context run))))

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


(defun parse-environment ()
  (let* ((env (e:make-env-reader))
         ;; TODO: we should use run-ctx for all our computations, and
         ;; eventually remove the setf-ing of flags. See T795
         (run-ctx (make-instance 'flags-run-context
                                 :env env)))
    (when flags:*branch*
      (error "--branch is no longer supported, please use --main-branch instead"))

    ;; TODO: we shouldn't refer to the flags from this point onwards,
    ;; instead just using the run-context.

    (setf flags:*build-url*
          (run-context:build-url run-ctx))

    (setf flags:*repo-url*
          (run-context:repo-url run-ctx))

    (setf flags:*work-branch*
          (run-context:work-branch run-ctx))

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
  ;; Heads up! This logic is not unit-tested. If you're modifying this
  ;; code, run `make sdk-integration-tests`.
  (let ((repo (git-repo))
        (commit-graph-updater (make-instance 'commit-graph-updater :api-context api-context))
        (branch flags:*main-branch*))
    (log:info "Uploading images from: ~a" directory)
    (make-directory-run api-context directory
                        :channel channel
                        :before (lambda (run-context)
                                  (when (and flags:*production*
                                             (> flags:*commit-limit* 0))
                                    (update-commit-graph commit-graph-updater repo branch
                                                         :override-commit-hash
                                                         (run-context:override-commit-hash run-context))))
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
