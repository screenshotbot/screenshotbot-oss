;; Copyright 2018-Present Modern Interpreters Inc.
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(uiop:define-package :screenshotbot/sdk/sdk
    (:nicknames :screenshotbot-sdk)
    (:use #:cl
          #:com.google.flag
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
  (:import-from #:screenshotbot/sdk/help
                #:help)
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
  (:export
   #:main))

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

(defun request (api &key (method :post)
                      parameters)
  (log:debug "Making API request: ~S, with parameters ~S" api parameters)
  (when (and (eql method :get) parameters)
    (error "Can't use :get with parameters"))
  (with-open-stream (stream
                (dex:request (format nil "~a~a" *hostname* api)
                             :want-stream t
                             :method method
                             :content (apply 'list
                                             (cons "api-key" *api-key*)
                                             (cons "api-secret-key" *api-secret*)
                                             parameters)))
    (let ((result (json:decode-json stream)))
      (awhen (assoc-value result :error)
        (log:error "API error: ~a" it)
        (error 'api-error :message it))
      (assoc-value result :response))))


(defun put-file (upload-url stream)
  (restart-case
      (multiple-value-bind (result code)
          (uiop:with-temporary-file (:stream tmp-stream :pathname tmpfile :direction :io
                                     :element-type 'flexi-streams:octet)
            (uiop:copy-stream-to-stream stream tmp-stream
                                        :element-type 'flexi-streams:octet)
            (finish-output tmp-stream)
            (file-position tmp-stream 0)
            (log:info "Got file length: ~a" (file-length tmp-stream))
            (dex:put upload-url
                     :headers `((:content-type . "application/octet-stream")
                                ;; There a bug in dexador that prevents the
                                ;; file-length logic to work correctly in
                                ;; delivered LW images
                                (:content-length . ,(file-length tmp-stream)))
                     :content tmpfile))

     (log:debug "Got image upload response: ~s" result)
     (unless (eql 200 code)
       (error "Failed to upload image: code ~a" code))
     result)
    (re-attempt-put-file ()
      (file-position stream 0)
      (put-file upload-url stream))))

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
                   branch
                   create-github-issue
                   (metadata-provider  (make-instance 'metadata-provider))
                   is-trunk)
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
       (let* ((branch-hash (rev-parse repo branch))
              (commit (current-commit repo))
              (merge-base (merge-base repo branch-hash commit))
              (response (request "/api/run"
                                :parameters `(("channel" . ,channel)
                                              ("screenshot-records" . ,records)
                                              ("branch" . ,branch)
                                              ("branch-hash" . ,branch-hash)
                                              ("github-repo" . ,(repo-link repo))
                                              ("merge-base" . ,merge-base)
                                              ("build-url" . ,*build-url*)
                                              ("pull-request" . ,pull-request)
                                              ("commit" . ,commit)
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

(defmethod upload-image-directory ((bundle image-directory))
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
     (make-image-bundle *directory*
                                   :metadata *metadata*))
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

(defun parse-environment ()
  (setf *branch* (or *main-branch* *branch*))
  (unless *pull-request*
    (setf *pull-request*
          (or
           (uiop:getenv "CIRCLE_PULL_REQUEST")
           (ignore-errors ;; temporary, until we're sure this works correctly
            (if-let ((repo-url (getenv "BITRISEIO_PULL_REQUEST_REPOSITORY_URL"))
                     (pull-id (getenv "BITRISE_PULL_REQUEST")))
              (format nil "~a/pulls/~a"
                      repo-url
                      pull-id))))))
  (unless *branch*
    (setf *branch*
          (or
           (uiop:getenv "CIRCLE_BRANCH")
           "master"))))

(defun parse-build-url ()
  (setf *build-url*
        (or *build-url*
            (uiop:getenv "BUILD_URL") ;; jenkins
            (uiop:getenv "CIRCLE_BUILD_URL")
            (uiop:getenv "BITRISE_BUILD_URL"))))

(defun parse-org-defaults ()
  (parse-build-url)
  (when (str:emptyp *api-key*)
    (error "No --api-key provided"))
  (when( str:emptyp *api-secret*)
    (error "No --api-secret provided"))
  (parse-environment)
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

(defun absolute-pathname (p)
  (fad:canonical-pathname (path:catdir (uiop:getcwd) p)))

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

(defun single-directory-run (directory &key channel)
  (log:info "Uploading images from: ~a" directory)
  (let ((repo (make-instance 'git-repo
                             :link *repo-url*))
        (branch *branch*))
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



(defun %main (&optional (argv #+lispworks system:*line-arguments-list*
                              #-lispworks nil))
  (log4cl:reset-logging-configuration)
  (log:config :info)
  (log:info "Screenshotbot SDK v2.3.3")
  (let ((unrecognized   (parse-command-line (cdr (command-line)))))
    (when *verbose*
      (log:config :debug))
    (log:debug "Run this in interactive shell: ~S"
               `(progn
                  (chdir-for-bin ,(uiop:getcwd))
                  (main ',argv)))
    (cond
      (unrecognized
       (format t "Unrecognized arguments: ~a~%" (Str:join " " unrecognized))
       (help)
       (uiop:quit 1))
      (*help*
       (help))
      (*ios-multi-dir*
       (parse-org-defaults)
       (let ((*directory* (namestring (absolute-pathname *directory*))))
         (loop for directory in (recursive-directories *directory*)
               do
                  (upload-ios-subdirectory directory))))
      (t
       (parse-org-defaults)
       (prepare-directory
        (lambda (directory)
          (single-directory-run directory :channel *channel*)))))))

(defun main (&rest args)
  (handler-bind ((warning (lambda (warning)
                            (let ((msg (princ-to-string warning)))
                              ;; This warning is not very actionable
                              ;; for end-users, so let's muffle it
                              #+lispworks
                              (when (str:containsp "output-wait is not implemented" msg)
                                (muffle-warning warning))))))
    (apply '%main args)))
