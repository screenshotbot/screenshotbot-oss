;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/sdk/run-context
  (:use #:cl)
  (:import-from #:screenshotbot/sdk/git
                #:git-root
                #:current-commit
                #:rev-parse)
  (:import-from #:alexandria
                #:remove-from-plist
                #:when-let)
  (:import-from #:util/misc
                #:?.)
  (:import-from #:easy-macros
                #:def-easy-macro)
  (:import-from #:util/json-mop
                #:ext-json-serializable-class)
  (:export
   #:run-context
   #:flags-run-context
   #:main-branch
   #:pull-request-url
   #:create-github-issue-p
   #:productionp
   #:repo-url
   #:git-repo
   #:env-reader-run-context
   #:override-commit-hash
   #:channel
   #:main-branch-hash
   #:commit-hash
   #:merge-base
   #:repo-clean-p
   #:work-branch
   #:build-url
   #:compare-threshold
   #:batch
   #:gitlab-merge-request-iid
   #:phabricator-diff-id
   #:with-flags-from-run-context
   #:tags
   #:author
   #:default-flags-run-context
   #:shard-spec
   #:run-context-metadata
   #:work-branch-is-release-branch-p)
  (:local-nicknames (#:flags #:screenshotbot/sdk/flags)
                    (#:e #:screenshotbot/sdk/env)
                    (#:dto #:screenshotbot/api/model)                    
                    (#:git #:screenshotbot/sdk/git)))
(in-package :screenshotbot/sdk/run-context)

(defmacro wrap-dto ((def klass empty slots))
  (declare (ignore def empty))
  `(progn
     (defclass ,klass ()
       ,(loop for slot in slots
              collect
              (list*
               (car slot)
               (remove-from-plist (cdr slot) :json-type :json-key))))
     (defclass run-context-dto (,klass)
       ,slots
       (:metaclass ext-json-serializable-class))

     (defmethod run-context-to-dto (run-context)
       (make-instance 'run-context-dto
                      ,@ (loop for slot in slots
                               for slot-args = (cdr slot)
                               appending
                               `(,(getf slot-args :initarg)
                                 (,(getf slot-args :reader)
                                  run-context)))))))

(wrap-dto
 (defclass run-context ()
   ((main-branch :initarg :main-branch
                 :initform nil
                 :reader main-branch
                 :json-type (or null :string)
                 :json-key "mainBranch")
    (main-branch-hash :initarg :main-branch-hash
                      :initform nil
                      :reader main-branch-hash
                      :json-type (or null :string)
                      :json-key "mainBranchCommit")
    (release-branch-regex :initarg :release-branch-regex
                          :initform nil
                          :reader release-branch-regex
                          :json-type (or null :string)
                          :json-key "releaseBranchRegex")
    (pull-request-url :initarg :pull-request-url
                      :initform nil
                      :reader pull-request-url
                      :json-type (or null :string)
                      :json-key "pullRequestUrl")
    (create-github-issue-p :initarg :create-github-issue-p
                           :initform nil
                           :reader create-github-issue-p
                           :json-type (or null :string)
                           :json-key "shouldCreateGithubIssue")
    (repo-url :initarg :repo-url
              :initform nil
              :reader repo-url
              :json-type (or null :string)
              :json-key "repo")
    (author :initarg :author
            :initform nil
            :reader author
            :json-type (or null :string)
            :json-key "author")
    (productionp :initarg :productionp
                 :initform nil
                 :reader productionp
                 :json-type (or null :string)
                 :json-key "isTrunk")
    (work-branch :initarg :work-branch
                 :initform nil
                 :reader work-branch
                 :json-type (or null :string)
                 :json-key "workBranch")
    (build-url :initarg :build-url
               :initform nil
               :reader build-url
               :json-type (or null :string)
               :json-key "buildUrl")
    (gitlab-merge-request-iid :initarg :gitlab-merge-request-iid
                              :initform nil
                              :reader gitlab-merge-request-iid
                              :json-type (or null :string)
                              :json-key "gitlabMergeRequestIID")
    (phabricator-diff-id :initarg :phabricator-diff-id
                         :initform nil
                         :reader phabricator-diff-id
                         :json-type (or null :string)
                         :json-key "phabricatorDiff")
    (channel :initarg :channel
             :initform nil
             :reader channel
             :json-type (or null :string)
             :json-key "channel")
    (override-commit-hash :initarg :override-commit-hash
                          :initform nil
                          :reader override-commit-hash
                          :json-type (or null :string)
                          :json-key "overrideCommitHash")
    (commit-hash :initarg :commit-hash
                 :initform nil
                 :reader commit-hash
                 :json-type (or null :string)
                 :json-key "commit")
    (merge-base :initarg :merge-base
                :initform nil
                :reader merge-base
                :json-type (or null :string)
                :json-key "mergeBase")
    (repo-clean-p :initarg :repo-clean-p
                  :reader repo-clean-p
                  :documentation "No initform!"
                  :json-type :boolean
                  :json-key "isClean")
    (compare-threshold :initarg :compare-threshold
                       :initform nil
                       :reader compare-threshold
                       :json-type :number
                       :json-key "compareThreshold")
    (batch :initarg :batch
           :initform nil
           :reader batch
           :json-type (or null :string)
           :json-key "batch")
    (shard-spec :initarg :shard-spec
                :initform nil
                :reader shard-spec
                :documentation "A dto:shard-spec object indicating the shard"
                :json-type (or null :string)
                :json-key "shardSpec")
    (metadata :initarg :metadata
              :initform nil
              :reader run-context-metadata
              :documentation "A list of dto:metadata objects"
              :json-type (:list dto:metadata)
              :json-key "metadata")
    (tags :initarg :tags
          :initform nil
          :reader tags
          :json-type (:list :string)
          :json-key "tags"))))

(defclass env-reader-run-context ()
  ((env :initarg :env
        :reader env)))

(defmethod %git-repo ((self env-reader-run-context) &key repo-url)
  "This function is here to break a cyclical dependency between git-repo and repo-url"
  (cond
    ((git-root :errorp nil)
     (make-instance 'git:git-repo :link repo-url))
    (t
     (log:warn "This is not running inside a Git repo. Please contact support@screenshotbot.io for advice, since the behavior in this case can be very different.")
     (make-instance 'git:null-repo))))

(defmethod git-repo ((self env-reader-run-context))
  (%git-repo self :repo-url (repo-url self)))

(defmethod author :around ((self env-reader-run-context))
  (or
   (call-next-method)
   (git:author (git-repo self))))

(defmethod build-url :around ((self env-reader-run-context))
  (or
   (call-next-method)
   (e:build-url (env self))))

(defmethod repo-url :around ((self env-reader-run-context))
  (or
   (call-next-method)
   (e:repo-url (env self))
   ;; The Git repository URL might have temporary authentication
   ;; information, for example on GitLab CI. We might want to remove
   ;; this before we default to the Git repository URL instead of from
   ;; the environment.
   (git:get-remote-url (%git-repo self))))

(defmethod work-branch :around ((self env-reader-run-context))
  (or
   (call-next-method)
   (e:work-branch (env self))))

(defmethod run-context-metadata :around ((self env-reader-run-context))
  (append
   (call-next-method)
   (list
    (make-instance 'dto:metadata
                   :key "pull-request-base-branch"
                   :value (or
                           (e:pull-request-base-branch (env self))
                           "NA"))
    #+ (or linux darwin)
    (make-instance 'dto:metadata
                   :key "uname"
                   :value
                   (or
                    (ignore-errors
                     (uiop:run-program (list "uname" "-a")
                                       :output 'string))
                    "uname failed")))))

(define-condition invalid-pull-request (condition)
  ())


(defun validate-pull-request (pull-request)
  "One of our customers is using an incorrect --pull-request arg. The
incorrect arg breaks some logic, and additionally is not required
since we can determine the pull-request from the environment. We can
do a quick sanity check, and recover with a warning if the
pull-request looks incorrect."
  (flet ((validp (url)
           (str:starts-with-p "https://" url)))
    (cond
      ((and
        pull-request
        (not (validp pull-request)))
       (signal 'invalid-pull-request)
       (log:warn "The --pull-request argument you provided was invalid: `~a`. We're ignoring this.~%"
                 flags:*pull-request*)
       nil)
      (t
       pull-request))))

(defmethod pull-request-url :around ((self env-reader-run-context))
  (or
   (validate-pull-request
    (call-next-method))
   (e:pull-request-url (env self))))

(defmethod channel :around ((self env-reader-run-context))
  (let ((channel (call-next-method)))
    (cond
      ((and
        (or
         (not channel)
         (equal "unnamed-channel" channel))
        (let ((e-channel (e:guess-channel-name (env self))))
          (cond
            (e-channel e-channel)
            (t channel)))))
      (t
       channel))))

(defmethod main-branch :around ((self env-reader-run-context))
  (or
   (call-next-method)
   (guess-master-branch (git-repo self))))

(defmethod work-branch-is-release-branch-p ((self run-context))
  (unless (str:emptyp (release-branch-regex self))
    (handler-case
        (cl-ppcre:parse-string (release-branch-regex self))
      (cl-ppcre:ppcre-syntax-error (e)
        (error "Could not parse regex: ~% ~a~%Got error:~% ~a" (release-branch-regex self) e)))
    (let ((regex (format nil "^~a$" (release-branch-regex self))))
      (not (null (cl-ppcre:scan regex
                                (work-branch self)))))))

(defmethod override-commit-hash :around ((self env-reader-run-context))
  (or
   (call-next-method)
   (e:sha1 (env self))))

(defmethod commit-hash :around ((self env-reader-run-context))
  (or
   (call-next-method)
   (?. current-commit (git-repo self))))

(defmethod merge-base :around ((self env-reader-run-context))
  (or
   (call-next-method)
   (when-let ((repo (git-repo self))
              (main-branch-hash (main-branch-hash self)))
     (handler-case
         (git:merge-base repo main-branch-hash (commit-hash self))
       (uiop:subprocess-error (e)
         (warn "merge-base computation failed: ~a" e)
         nil)))))

(defmethod repo-clean-p :around ((self env-reader-run-context))
  (cond
    ((slot-boundp self 'repo-clean-p)
     (call-next-method))
    (t
     (git:cleanp (git-repo self)))))

(defmethod guess-master-branch (repo)
  (flet ((check (x)
           (rev-parse repo x)))
    (cond
      ((check "main")
       "main")
      ((check "master")
       "master")
      (t
       (error "Could not guess the main branch, please use --main-branch argument")))))

(defmethod guess-master-branch ((repo git:null-repo))
  nil)

(defmethod main-branch ((self env-reader-run-context))
  (or
   (call-next-method)
   (guess-master-branch (git-repo self))))

(defmethod main-branch-hash ((Self env-reader-run-context))
  (or
   (call-next-method)
   (let ((branch (main-branch self)))
    (when-let ((hash (rev-parse (git-repo self) branch)))
      (unless hash
        (error "Could not rev-parse origin/~a" branch))
      hash))))

(defun parse-tags (tags-str)
  (when tags-str
    (str:split "," tags-str)))

(defun parse-shard-spec (shard-spec)
  "Parses a string of the form <buildId>:<index>:<count> into a dto:shard-spec"
  ;; TODO: better error messages when this fails
  (unless (str:emptyp shard-spec)
    (destructuring-bind (key index count)
        (str:rsplit ":" shard-spec :limit 3)
      (make-instance 'dto:shard-spec
                     :key key
                     :number (parse-integer index)
                     :count (parse-integer count)))))

(defun format-shard-spec (shard-spec)
  (when shard-spec
   (format nil "~a:~a:~a"
           (dto:shard-spec-key shard-spec)
           (dto:shard-spec-number shard-spec)
           (dto:shard-spec-count shard-spec))))

(defclass default-flags-run-context ()
  ()
  (:default-initargs
   :main-branch flags:*main-branch*
   :main-branch-hash flags:*main-branch-commit-hash*
   :pull-request-url flags:*pull-request*
   :create-github-issue-p flags:*create-github-issue*
   :repo-url flags:*repo-url*
   :productionp flags:*production*
   :build-url flags:*build-url*
   :gitlab-merge-request-iid flags:*gitlab-merge-request-iid*
   :work-branch flags:*work-branch*
   :phabricator-diff-id flags:*phabricator-diff-id*
   :channel flags:*channel*
   :merge-base flags:*merge-base-commit-hash*
   :override-commit-hash flags:*override-commit-hash*
   :release-branch-regex flags:*release-branch-regex*
   :author flags:*author*
   :compare-threshold flags:*compare-threshold*
   :batch flags:*batch*
   :shard-spec (parse-shard-spec flags:*shard*)
   :tags (parse-tags flags:*tags*))
  (:documentation "Just uses the the flags as initargs"))

(defclass flags-run-context (default-flags-run-context
                             env-reader-run-context
                             run-context)
  ())

(def-easy-macro with-flags-from-run-context (self &fn fn)
  "Temporary hack to enable using both flags and run-context everywhere."
  (check-type self run-context)
  (let ((flags:*main-branch* (main-branch self))
        (flags:*main-branch-commit-hash* (main-branch-hash self))
        (flags:*pull-request* (pull-request-url self))
        (flags:*create-github-issue* (create-github-issue-p self))
        (flags:*repo-url* (repo-url self))
        (flags:*production* (productionp self))
        (flags:*build-url* (build-url self))
        (flags:*gitlab-merge-request-iid* (gitlab-merge-request-iid self))
        (flags:*phabricator-diff-id* (phabricator-diff-id self))
        (flags:*channel* (channel self))
        (flags:*override-commit-hash* (override-commit-hash self))
        (flags:*compare-threshold* (compare-threshold self))
        (flags:*batch* (batch self))
        (flags:*shard* (format-shard-spec (shard-spec self))))
    (fn)))

