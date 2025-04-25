;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/api/model
            (:use #:cl)
            (:import-from #:json-mop
                          #:json-serializable-class)
            (:import-from #:util/json-mop
                          #:ext-json-serializable-class)
            (:local-nicknames (#:a #:alexandria))
            (:export
             #:encode-json
             #:*api-version*
             #:version-number
             #:failed-run
             #:failed-run-channel
             #:failed-run-commit
             #:screenshot
             #:screenshot-name
             #:screenshot-image-id
             #:screenshot-lang
             #:screenshot-device
             #:screenshot-list
             #:run
             #:run-id
             #:run-channel
             #:run-screenshots
             #:run-commit
             #:should-create-github-issue-p
             #:trunkp
             #:periodic-job-p
             #:cleanp
             #:pull-request-url
             #:main-branch-hash
             #:merge-base
             #:commit
             #:main-branch
             #:gitlab-merge-request-iid
             #:phabricator-diff-id
             #:run-repo
             #:build-url
             #:override-commit-hash
             #:compare-threshold
             #:report
             #:work-branch
             #:unchanged-run-other-commit
             #:unchanged-run-commit
             #:unchanged-run-channel
             #:unchanged-run
             #:finalized-commit
             #:finalized-commit-hash
             #:recorder-run-url
             #:run-batch
             #:comparison
             #:comparison-samep
             #:comparison-title
             #:comparison-url
             #:run-tags
             #:run-author
             #:batch
             #:batch-repo
             #:batch-commit
             #:batch-name
             #:screenshot-url
             #:shard-spec-count
             #:shard-spec-number
             #:shard-spec-key
             #:shard-spec
             #:image-upload-response
             #:image-md5sum
             #:image-upload-url
             #:image-id
             #:metadata
             #:run-metadata
             #:metadata-key
             #:metadata-value
             #:release-branch-p
             #:report-id
             #:report-acceptable-state
             #:report-run-id
             #:report-previous-run-id
             #:report-title
             #:commit-graph))

(in-package :screenshotbot/api/model)

;; Please update CHANGELOG.md
(defparameter *api-version* 17)

(defclass version ()
  ((version :initarg :version
            :json-key "version"
            :json-type :number
            :reader version-number)
   (url :initarg :url
        :json-key "url"
        :json-type (or null :string)
        :reader installation-url
        :documentation "The installation's URL"))
  (:metaclass ext-json-serializable-class))

(defmethod encode-json (object)
  (with-output-to-string (out)
    (json-mop:encode object out)))

(defmethod decode-json (json type)
  (declare (optimize (speed 0) (debug 3)))
  (cond
    ((and
      (listp type)
      (eql 'or (first type))
      (eql 'null (second type)))
     (cond
       ((equal "null" json)
        nil)
       (t
        (decode-json json (third type)))))
    ((and
      (listp type)
      (eql :list (car type)))
     (loop for item across (yason:parse (make-string-input-stream json)
                                        :object-as :hash-table
                                        :json-arrays-as-vectors t
                                        :json-booleans-as-symbols t
                                        :json-nulls-as-keyword t)
           collect (json-mop:json-to-clos item (second type))))
    (t
     (json-mop:json-to-clos json type))))

(defclass failed-run ()
  ((id :initarg :id
       :json-type (or null :number)
       :initform nil
       :json-key "id")
   (channel :initarg :channel
            :json-key "channel"
            :json-type (or null :string)
            :initform nil
            :reader failed-run-channel)
   (commit :initarg :commit
           :json-key "commit"
           :json-type (or null :string)
           :initform nil
           :reader failed-run-commit))
  (:metaclass ext-json-serializable-class))

(defclass abstract-run-dto ()
  ((batch :initarg :batch
          :json-key "batch"
          :initform nil
          :json-type (or null :string)
          :reader run-batch
          :documentation "The batch name associated with this run")
   (repo :initarg :github-repo
         :json-key "repo"
         :json-type (or null :string)
         :initform nil
         ;; Internally this is github-repo :/
         :reader run-repo
         :documentation "The repository URL")
   (override-commit-hash :initarg :override-commit-hash
                         :json-key "overrideCommitHash"
                         :json-type (or null :string)
                         :initform nil
                         :reader override-commit-hash
                         :documentation "The Git hash associated with the current run. This might be different from `commit` if the CI job had a step of rebasing the changes onto the master branch.")
   (pull-request-url :initarg :pull-request
                     :json-key "pullRequestUrl"
                     :json-type (or null :string)
                     :initform nil
                     :reader pull-request-url
                     :documentation "The pull request URL associated with this run, if any.")
   (phabrictor-diff-id :initarg :phabricator-diff-id
                       :json-key "phabricatorDiff"
                       :json-type (or null :number)
                       :initform nil
                       :reader phabricator-diff-id
                       :documentation "A Phabricator Diff ID associated with the run, if any.")
   (work-branch :initarg :work-branch
                :json-key "workBranch"
                :json-type (or null :string)
                :initform nil
                :reader work-branch
                :documentation "The branch on which the CI job was run")
   (release-branch-p :initarg :release-branch-p
                     :json-key "isReleaseBranch"
                     :json-type (or null :bool)
                     :initform nil
                     :reader release-branch-p
                     :documentation "Is the work-branch a release branch, typically this
is computed on the CLI via a regex.")
   (merge-base :initarg :merge-base
               :json-key "mergeBase"
               :json-type (or null :string)
               :initform nil
               :reader merge-base
               :documentation "The commit hash of the merge base of this commit with the main branch."))
  (:metaclass ext-json-serializable-class))

(defclass unchanged-run (abstract-run-dto)
  ((id :initarg :id
       :json-type (or null :number)
       :initform nil
       :json-key "id")
   (channel :initarg :channel
            :json-key "channel"
            :json-type (or null :string)
            :initform nil
            :reader unchanged-run-channel)
   (commit :initarg :commit
           :json-key "commit"
           :json-type (or null :string)
           :initform nil
           :reader unchanged-run-commit
           :reader run-commit)
   (other-commit :initarg :other-commit
                 :json-key "other-commit"
                 :json-type (or null :string)
                 :initform nil
                 :reader unchanged-run-other-commit))
  (:metaclass ext-json-serializable-class))

(defclass finalized-commit ()
  ((id :initarg :id
       :json-type (or null :number)
       :initform nil
       :json-key "id")
   (commit :initarg :commit
           :json-key "commit"
           :json-type (or null :string)
           :reader finalized-commit-hash))
  (:metaclass ext-json-serializable-class))

(defclass batch ()
  ((id :initarg :id
       :json-type (or null :number)
       :initform nil
       :json-key "id")
   (repo :initarg :github-repo
         :json-key "repo"
         :json-type (or null :string)
         :initform nil
         :reader batch-repo
         :documentation "The repository URL")
   (commit :initarg :commit
           :json-key "commit"
           :json-type (or null :string)
           :reader batch-commit)
   (name :initarg :name
         :json-key "name"
         :json-type :string
         :reader batch-name)
   (phabrictor-diff-id :initarg :phabricator-diff-id
                       :json-key "phabricatorDiff"
                       :json-type (or null :number)
                       :initform nil
                       :reader phabricator-diff-id
                       :documentation "A Phabricator Diff ID associated with the run, if any.")
   (pull-request-url :initarg :pull-request
                     :json-key "pullRequestUrl"
                     :json-type (or null :string)
                     :initform nil
                     :reader pull-request-url
                     :documentation "The pull request URL associated with this run, if any."))
  (:metaclass ext-json-serializable-class))

(defclass screenshot ()
  ((name :initarg :name
         :json-type :string
         :json-key "name"
         :reader screenshot-name
         :documentation "The name associated with this screenshot")
   (image-id :initarg :image-id
             :json-type :string
             :json-key "imageId"
             :reader screenshot-image-id
             :documentation "The ID of the image associated with this screenshot")
   (url :initarg :url
        :json-type :string
        :json-key "url"
        :reader screenshot-url
        :documentation "The URL to download the original image from")
   (lang :initarg :lang
         :initform nil
         :json-type (or null :string)
         :json-key "lang"
         :reader screenshot-lang)
   (device :initarg :device
           :initform nil
           :json-type (or null :string)
           :json-key "device"
           :reader screenshot-device))
  (:metaclass ext-json-serializable-class))

(defclass shard-spec ()
  ((key :initarg :key
        :json-type :string
        :json-key "key"
        :reader shard-spec-key
        :documentation "A unique, but arbitrary, identifier that identifies all
the shards. For instance this might be the CircleCI root build id.")
   (number :initarg :number
           :json-type :number
           :json-key "number"
           :reader shard-spec-number
           :documentation "The number of the shard, 0-indexed.")
   (count :initarg :count
          :json-type :number
          :json-key "count"
          :reader shard-spec-count
          :documentation "The total count of shards"))
  (:documentation "If we're sending a run in multiple shards, this specifies the current
shard.")
  (:metaclass ext-json-serializable-class))

(defclass metadata ()
  ((key :initarg :key
        :reader metadata-key
        :json-type :string
        :json-key "key")
   (value :initarg :value
          :reader metadata-value
          :json-type :string
          :json-key "value"))
  (:documentation "An aribtrary key-value pair, typically used for storing debugging
information on runs.")
  (:metaclass ext-json-serializable-class))

(defclass run (abstract-run-dto)
  ((id :initarg :id
       :json-key "id"
       :json-type :string
       :reader run-id
       :documentation "The ID of this run")
   (channel :initarg :channel
            :json-key "channel"
            :json-type :string
            :reader run-channel
            :documentation "The channel name used with this run")
   (screenshots :initarg :screenshots
                :json-key "screenshots"
                :json-type (:list screenshot)
                :reader run-screenshots
                :documentation "A list of screenshots for this run. This field may not be present when querying a run.")
   (commit :initarg :commit-hash
           :json-key "commit"
           :json-type (or null :string)
           :initform nil
           :reader run-commit
           :documentation "The Git commit hash for this run")
   (create-github-issue :initarg :create-github-issue-p
                        :json-key "shouldCreateGithubIssue"
                        :initform nil
                        :json-type :bool
                        :reader should-create-github-issue-p)
   (trunkp :initarg :trunkp
           :json-key "isTrunk"
           :json-type :bool
           :initform nil
           :reader trunkp)
   (periodic-job-p
    :initarg :periodic-job-p
    :json-key "isPeriodicJob"
    :json-type :bool
    :initform nil
    :reader periodic-job-p)
   (cleanp
    :initarg :cleanp
    :json-key "isClean"
    :json-type :bool
    :initform nil
    :reader cleanp)
   (main-branch-hash :initarg :main-branch-hash
                     :json-key "mainBranchCommit"
                     :json-type (or null :string)
                     :initform nil
                     :reader main-branch-hash
                     :documentation "The Git hash of the main branch at the time that this run was created.")
   (build-url :initarg :build-url
              :json-key "buildUrl"
              :json-type (or null :string)
              :initform nil
              :reader build-url
              :documentation "The URL of the build job that created this run")
   (main-branch :initarg :main-branch
                :json-key "mainBranch"
                :json-type (or null :string)
                :initform nil
                :reader main-branch
                :documentation "The main branch, usually `main` or `master`.")
   (gitlab-merge-request-iid :initarg :gitlab-merge-request-iid
                             :json-key "gitlabMergeRequestIID"
                             :json-type (or null :number)
                             :initform nil
                             :reader gitlab-merge-request-iid
                             :documentation "A GitLab merge request IID associated with the run, if any.")
   (compare-threshold :initarg :compare-threshold
                      :json-key "compareThreshold"
                      :json-type (or null :number)
                      :initform nil
                      :reader compare-threshold
                      :documentation "The comparison threshold used for comparisons associated with this run.")
   (url :initarg :url
        :json-key "url"
        :json-type (or null :string)
        :reader recorder-run-url
        :documentation "The URL of this run")
   (tags :initarg :tags
         :json-key "tags"
         :initform nil
         :json-type (:list :string)
         :reader run-tags
         :documentation "A list of arbitrary tags associated with this run")
   (author :initarg :author
           :reader run-author
           :json-key "author"
           :initform nil
           :json-type (or null :string)
           :documentation "The author of this run. This is used when implementing policies around reviews.")
   (metadata :initarg :metadata
             :initform nil
             :reader run-metadata
             :json-key "metadata"
             :json-type (:list metadata)
             :documentation "A list of metadata elements. This information will typically not affect
runs, but only for debugging.")
   (shard-spec :initarg :shard-spec
               :reader shard-spec
               :json-key "shard"
               :initform nil
               :json-type (or null shard-spec)
               :documentation "An optional shard-spec. This information is not present when reading a run. When creating a run, the presence of a shard-spec might delay the actual creation until all of the shards are available."))
  (:metaclass ext-json-serializable-class))

(defclass report ()
  ((id :initarg :id
       :json-key "id"
       :json-type :string
       :reader report-id
       :documentation "The ID of this report")
   (run :initarg :run
        :json-key "run"
        :json-type :string
        :reader report-run-id
        :documentation "The ID of the run that generated this")
   (channel :initarg :channel
            :json-key "channel"
            :json-type (or null :string)
            :documentation "The channel name")
   (title :initarg :title
          :json-key "title"
          :json-type (or null :string)
          :reader report-title
          :documentation "The report title, something like `N changes, M added`")
   (previous-run :initarg :previous-run
                 :json-key "previousRun"
                 :json-type (or null :string)
                 :reader report-previous-run-id)
   (review-state :initarg :acceptable-state
                 :json-key "reviewState"
                 :json-type :string
                 :reader report-acceptable-state
                 :documentation "The review status of the report. One of 'accepted', 'rejected', 'none' or 'na'.")
   (reviewer-name :initarg :reviewer-name
                  :json-key "reviewerName"
                  :json-type (or null :string)
                  :documentation "If the review state 'accepted' or 'rejected', this will be the name of the reviewer"))
  (:metaclass ext-json-serializable-class))

(defmethod json-mop:json-to-clos ((items vector) (class (eql 'screenshot-list))
                                  &rest initargs)
  (loop for x across items
        collect (apply #'json-mop:json-to-clos x 'screenshot initargs)))

(defclass comparison ()
  ((samep :initarg :samep
          :json-key "isSame"
          :json-type :boolean
          :reader comparison-samep)
   (changes :initarg :changes
            :json-key "changes"
            :json-type (:list :string)
            :reader comparison-changes
            :documentation "List of names of all screenshots that were changed")
   (added :initarg :added
          :json-key "added"
          :json-type (:list :string)
          :reader comparison-added
          :documentation "List of names of screenshots that were added")
   (deleted :initarg :deleted
            :json-key "deleted"
            :json-type (:list :string)
            :documentation "List of names of screenshots that were deleted")
   (channel :initarg :channel
            :json-key "channel"
            :json-type (or null :string)
            :documentation "The channel name. If the runs being compared are from different channels then this field is undefined.")
   (title :initarg :title
          :json-key "title"
          :json-type (or null :string)
          :reader comparison-title)
   (url :initarg :url
        :json-key "url"
        :json-type (or null :string)
        :reader comparison-url))
  (:metaclass ext-json-serializable-class))

(defclass image-upload-response ()
  ((image-id :initarg :image-id
             :reader image-id
             :json-key "imageId"
             :json-type :string)
   (md5sum :initarg :md5sum
           :reader image-md5sum
           :json-key "md5sum"
           :json-type :string)
   (upload-url :initarg :upload-url
               :accessor image-upload-url
               :json-key "uploadUrl"
               :json-type (or null :string)))
  (:metaclass ext-json-serializable-class)
  (:documentation "TODO: at time of writing, we're not sending this over the API, and
it's just being constructed in the SDK. This will be used to send a
response back from /api/screenshot."))

(defclass commit ()
  ((sha :initarg :sha
        :json-key "sha"
        :json-type :string)
   (parents :initarg :parents
            :json-key "parents"
            :json-type (:list :string)))
  (:metaclass ext-json-serializable-class))

(defclass git-ref ()
  ((name :initarg :name
         :json-key "name"
         :json-type :string)
   (sha :initarg :sha
        :json-key "sha"
        :json-type :string))
  (:metaclass ext-json-serializable-class))

(defclass commit-graph ()
  ((commits :initarg :commits
            :json-key "commits"
            :json-type (:list commit))
   (repo :initarg :repo
         :json-key "repo"
         :json-type :string)
   (refs :initarg :refs
         :json-key "refs"
         :json-type (:list git-ref)))
  (:metaclass ext-json-serializable-class))
