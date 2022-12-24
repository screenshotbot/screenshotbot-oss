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
        #:screenshotbot/sdk/flags)
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
  (:import-from #:screenshotbot/sdk/android
                #:make-image-bundle
                #:android-run-p)
  (:import-from #:screenshotbot/sdk/git
                #:git-repo
                #:current-commit
                #:rev-parse
                #:read-graph
                #:cleanp
                #:repo-link
                #:merge-base)
  (:import-from #:uiop
                #:getenv)
  (:import-from #:util/request
                #:http-request)
  (:import-from #:util/misc
                #:or-setf)
  (:import-from #:screenshotbot/sdk/version-check
                #:remote-supports-basic-auth-p)
  (:local-nicknames (#:flags #:screenshotbot/sdk/flags))
  (:export
   #:single-directory-run
   #:*request*
   #:*put*
   #:request
   #:put-file
   #:parse-org-defaults
   #:run-ios-multi-dir-toplevel
   #:run-prepare-directory-toplevel
   #:absolute-pathname
   #:update-commit-graph))

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
  (awhen (assoc-value result :error)
    (log:error "API error: ~a" it)
    (error 'api-error :message it))
  (assoc-value result :response))

(defun backoff (num)
  (let ((ret (or
              (car (nthcdr num (list 10 20 60)))
              60)))
    (log:info "Will retry in ~a seconds" ret)
    ret))

(auto-restart:with-auto-restart (:retries 3 :sleep #'backoff)
  (defun %request (api &key (method :post)
                         parameters)
    ;; TODO: we're losing the response code here, we need to do
    ;; something with it.
    (uiop:slurp-input-stream
     'string
     (http-request
      (format nil "~a~a" *hostname* api)
      :method method
      :want-stream t
      :method method
      :basic-authorization (when (remote-supports-basic-auth-p)
                             (list
                              *api-key*
                              *api-secret*))
      :parameters (cond
                    ((remote-supports-basic-auth-p)
                     parameters)
                    (t (list*
                        (cons "api-key" *api-key*)
                        (cons "api-secret-key" *api-secret*)
                        parameters)))))))

(defun request (api &key (method :post)
                      parameters)
  (log:debug "Making API request: ~S" api)
  (when (and (eql method :get) parameters)
    (error "Can't use :get with parameters"))
  (let ((json (%request api :method method
                        :parameters parameters)))
    (handler-case
        (let ((result (json:decode-json-from-string json)))
          (ensure-api-success result))
      (json:json-syntax-error (e)
        (error "Could not parse json:"
               json)))))

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

(auto-restart:with-auto-restart (:retries 3 :sleep #'backoff)
  (defun put-file (upload-url stream &key parameters)
    ;; In case we're retrying put-file, let's make sure we reset the
    ;; stream
    (file-position stream 0)
    (with-file-stream (stream stream)
     (let ((file-length (file-length stream)))
       (log:debug "Got file length: ~a" file-length)
       (multiple-value-bind (result code)
         (http-request
          upload-url
          :method :put
          :parameters parameters
          :content-type "application/octet-stream"
          :content-length file-length
          :content stream)

         (log:debug "Got image upload response: ~s" result)
         (unless (eql 200 code)
           (error "Failed to upload image: code ~a" code))
         result)))))

(defun upload-image (key stream hash response)
  (Assert hash)
  (log:debug "Checking to see if we need to re-upload ~a, ~a" key hash)
  (log:debug "/api/screenshot response: ~s" response)
  (let ((upload-url (assoc-value response :upload-url)))
    (when upload-url
      (log:debug "Uploading ~a" key)
      (put-file upload-url stream)))
  (setf (assoc-value response :name) key)
  response)

(defun make-run (images &rest args
                 &key repo
                   channel
                   pull-request
                   (gitp t)
                   branch
                   (branch-hash nil has-branch-hash-p)
                   (commit nil has-commit-p)
                   (merge-base nil has-merge-base-p)
                   (github-repo nil has-github-repo-p)
                   periodic-job-p
                   create-github-issue
                   (metadata-provider  (make-instance 'metadata-provider))
                   is-trunk)
  (declare (ignore gitp))
  (restart-case
   (flet ((bool (x) (if x "true" "false")))
     (let ((records (json:encode-json-to-string
                     (loop for im in images collect
                           (let ((name (assoc-value im :name)))
                            `(("name" . ,name)
                              ("imageId" . ,(assoc-value im :id))
                              ("lang" . ,(screenshot-lang metadata-provider name))
                              ("device" . ,(screenshot-device metadata-provider name))))))))
       (log:debug "records: ~s" records)
       (let* ((branch-hash (if has-branch-hash-p branch-hash (rev-parse repo branch)))
              (commit (if has-commit-p commit (current-commit repo)))
              (merge-base (if has-merge-base-p merge-base (merge-base repo branch-hash commit)))
              (github-repo (if has-github-repo-p
                               github-repo
                               (repo-link repo)))
              (response (request "/api/run"
                                :parameters `(("channel" . ,channel)
                                              ("screenshot-records" . ,records)
                                              ("branch" . ,branch)
                                              ("branch-hash" . ,branch-hash)
                                              ("github-repo" . ,github-repo)
                                              ("merge-base" . ,merge-base)
                                              ("periodic-job-p" . ,(bool periodic-job-p))
                                              ("build-url" . ,*build-url*)
                                              ("pull-request" . ,pull-request)
                                              ("commit" . ,commit)
                                              ("override-commit-hash" . ,flags:*override-commit-hash*)
                                              ("create-github-issue" . ,(bool create-github-issue))
                                              ("is-clean" . ,(bool (cleanp repo)))
                                              ("gitlab-merge-request-iid" .
                                                                          ,*gitlab-merge-request-iid*)
                                              ("phabricator-diff-id" . ,*phabricator-diff-id*)
                                              ("is-trunk" . ,(bool is-trunk))))))
         ;; what to do with response?
         response)))
    (retry-run ()
      (apply 'make-run images
             args))))


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

(defclass metadata-provider ()
  ())

(defun read-first-match (regex name)
  (multiple-value-bind
        (full matches)
      (cl-ppcre:scan-to-strings regex name)
    (cond
      (full
       (elt matches 0))
      (t
       (log:debug "No regex match for ~a, ~a" regex name)))))

(defmethod screenshot-lang ((m metadata-provider) name)
  (when *lang-regex*
    (read-first-match *lang-regex* name)))

(defmethod screenshot-device ((m metadata-provider) name)
  (when *device-regex*
    (read-first-match *device-regex* name)))


(defmethod make-directory-run (dir &rest args)
  (log:debug "Reading images from ~a" dir)
  (let ((images
          (upload-image-directory dir)))
    (log:info "Creating run")
    (apply 'make-run images
           args)))
;;; let ((response (request "/api/screenshot"
;;;                            :parameters `(("name" . ,key)
;;;                                          ("hash" . ,hash)))))

(defun keyword-except-md5 (identifier)
  ;; this is bug prone, but I know for a fact we don't have
  ;; 32 character long identifiers in the json :/
  (cond
   ((eql 32 (length identifier))
    (string identifier))
   (t
    (json:camel-case-to-lisp identifier))))

(defmethod upload-image-directory (bundle)
  (let ((images (list-images bundle)))
    (let ((hash-to-response
           (let ((json:*json-identifier-name-to-lisp* 'keyword-except-md5))
             (request "/api/screenshot"
                      :parameters `(("hash-list"
                                     . ,(json:encode-json-to-string (mapcar 'md5-sum images))))))))
      (log:debug "got full response: ~s" hash-to-response)
      (loop for im in (list-images bundle)
            collect
            (progn
              (with-open-stream (s (image-stream im))
                (unwind-protect
                    (upload-image (image-name im) s
                                  (md5-sum im)
                                  (assoc-value hash-to-response (md5-sum im)
                                               :test 'string=))
                  (close-image im))))))))

(defun test-upload ()
  (make-directory-run #P "/home/arnold/builds/ios-oss/Screenshots/_64/Kickstarter_Framework_iOSTests.ActivitiesViewControllerTests/"
                      :channel "test-channel"
                      :repo (make-instance 'git-repo
                                           :link "https://github.com/tdrhq/ios-oss"
                                           :dir #P "~/builds/ios-oss/")
                      :is-trunk t
                      :branch "master"))

(defun %read-directory-from-args (&key (ensure-diff-dir t))
  (cond
    ((android-run-p)
     (log:info "Looks like an Android run")
     (make-image-bundle :metadata *metadata*))
    ((not (str:emptyp *directory*))
     (unless (path:-d *directory*)
       (error "Not a directory: ~a. Did you miss --metadata if you intended to use a bundle.zip?" *directory*))
     (cond
       (*ios-diff-dir*
        (log:debug "Using diff dir: ~a" *ios-diff-dir*)
        (when ensure-diff-dir
          (assert (path:-d *ios-diff-dir*)))
        (make-instance 'image-directory-with-diff-dir
                       :directory *directory*
                       :diff-dir *ios-diff-dir*))
       (t
        (make-instance 'image-directory :directory *directory*))))
    (t
     (error "Unknown run type, maybe you missed --directory?"))))

(defun prepare-directory (fn)
  (funcall
   fn
   (%read-directory-from-args)))

(defun emptify (s)
  "If the string is empty, return nil"
  (if (str:emptyp s) nil s))

(defun parse-api-key-from-environment ()
  (setf *api-key*
        (or (emptify *api-key*)
            (uiop:getenv "SCREENSHOTBOT_API_KEY")))
  (setf *api-secret*
        (or (emptify *api-secret*)
            (uiop:getenv "SCREENSHOTBOT_API_SECRET")))

  (alexandria:when-let (hostname (uiop:getenv "SCREENSHOTBOT_API_HOSTNAME"))
    (setf flags:*hostname* hostname)))

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
  (parse-api-key-from-environment)

  (when *branch*
    (error "--branch is no longer supported, please use --main-branch instead"))

  (unless *pull-request*
    (setf *pull-request*
          (or
           (uiop:getenv "CIRCLE_PULL_REQUEST")
           (ignore-errors ;; temporary, until we're sure this works correctly
            (if-let ((repo-url (getenv "BITRISEIO_PULL_REQUEST_REPOSITORY_URL"))
                     (pull-id (getenv "BITRISE_PULL_REQUEST")))
              (link-to-github-pull-request repo-url pull-id))))))

  (or-setf
   flags:*override-commit-hash*
   (unless (str:emptyp *pull-request*)
     (or
      (uiop:getenv "CIRCLE_SHA1")
      (uiop:getenv "BITRISE_GIT_COMMIT"))))

  (unless *main-branch*
    (setf *main-branch*
          (guess-master-branch (git-repo)))))

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

(defun parse-build-url ()
  (setf *build-url*
        (or *build-url*
            (uiop:getenv "BUILD_URL") ;; jenkins
            (uiop:getenv "CIRCLE_BUILD_URL")
            (uiop:getenv "BITRISE_BUILD_URL"))))

(defun maybe-parse-netlify-environment ()
  (when (equal "true" (uiop:getenv "NETLIFY"))
    (log:info "Looks like we're running in Netlify")
    (setf *repo-url*
          (uiop:getenv "REPOSITORY_URL"))
    (let ((build-id (uiop:getenv "BUILD_ID"))
          (site-name (uiop:getenv "SITE_NAME")))
      (setf *build-url*
            (format nil "https://app.netlify.com/sites/~a/deploys/~a"
                    site-name
                    build-id)))
    (let ((pull-request-p (equal "true" (uiop:getenv "PULL_REQUEST"))))
      (when pull-request-p
        (let ((review-id (uiop:getenv "REVIEW_ID")))
          (setf *pull-request*
                (link-to-github-pull-request
                 *repo-url*
                 review-id)))))
    (when (equal "unnamed-channel" *channel*)
      (setf *channel*
            (uiop:getenv "SITE_NAME")))))

(defun parse-org-defaults ()
  (parse-build-url)
  (parse-environment)
  (when (str:emptyp *api-key*)
    (error "No --api-key provided"))
  (when( str:emptyp *api-secret*)
    (error "No --api-secret provided"))
  (maybe-parse-netlify-environment)
  (when *org-defaults*
   (ecase (intern (str:upcase *org-defaults*) "KEYWORD")
     (nil
      nil)
     (:kickstarter-ios
      (setf *lang-regex* ".*_lang_(.*?)_.*")
      (setf *device-regex* ".*_device_(.*?)$")))))

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
                `(:relative ,@(subseq dir-parts (length parent-parts))))
               ))
      (log:debug "Relative path parts: ~S" (pathname-directory res))
      (log:debug "Relative path is: ~S" res)
      res)))

(defun upload-ios-subdirectory (directory)
  (log:info "Uploading run from ~a" directory)
  (let*((relative-dir (get-relative-path directory *directory*))
        (*directory* (namestring directory))
        (*ios-diff-dir* (when *ios-diff-dir*
                          (namestring (path:catdir *ios-diff-dir*
                                                   relative-dir)))))
    (log:info "Will look for image diffs at ~S" *ios-diff-dir*)
    (let ((dir (%read-directory-from-args :ensure-diff-dir nil)))
      (single-directory-run dir
                            :channel (format nil "~a/~a"
                                             *channel*
                                             (car (last (pathname-directory directory))))))))

(defmethod update-commit-graph (repo branch)
  (let* ((dag (read-graph repo))
         (json (with-output-to-string (s)
                 (dag:write-to-stream dag s))))
    (request "/api/commit-graph"
             :method :post
             :parameters (list
                          (cons "repo-url" (repo-link repo))
                          (cons "branch" branch)
                          (cons "graph-json" json)))))

(defun git-repo ()
  (make-instance 'git-repo
                  :link *repo-url*))

(defun single-directory-run (directory &key channel)
  (log:info "Uploading images from: ~a" directory)
  (let ((repo (git-repo))
        (branch *main-branch*))
    (when *production*
      (update-commit-graph repo branch))
    (make-directory-run directory
                        :channel channel
                        :pull-request *pull-request*
                        :create-github-issue *create-github-issue*
                        :repo repo
                        :is-trunk *production*
                        :branch branch)))

(defun chdir-for-bin (path)
  (uiop:chdir path)
  (setf *default-pathname-defaults* (pathname path)))


(defun absolute-pathname (p)
  (fad:canonical-pathname (path:catdir (uiop:getcwd) p)))

(defun run-ios-multi-dir-toplevel ()
  (let ((*directory* (namestring (absolute-pathname *directory*))))
    (loop for directory in (recursive-directories *directory*)
          do
             (upload-ios-subdirectory directory))))

(defun run-prepare-directory-toplevel ()
  (prepare-directory
   (lambda (directory)
     (single-directory-run directory :channel *channel*))))
