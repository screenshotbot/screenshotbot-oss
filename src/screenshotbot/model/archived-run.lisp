;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(uiop:define-package :screenshotbot/model/archived-run
  (:use #:cl
        #:alexandria)
  (:import-from #:util/json-mop
                #:ext-json-serializable-class)
  (:import-from #:bknr.indices
                #:base-indexed-object)
  (:export
   #:archived-run
   #:archived-run-channel
   #:archived-run-company
   #:archived-run-commit
   #:archived-run-build-url
   #:archived-run-github-repo
   #:archived-run-cleanp
   #:archived-run-activep
   #:archived-run-branch
   #:archived-run-work-branch
   #:archived-run-release-branch-p
   #:archived-run-branch-hash
   #:archived-run-merge-base
   #:archived-run-pull-request-url
   #:archived-run-gitlab-merge-request-iid
   #:archived-run-phabricator-diff-id
   #:archived-run-previous-run
   #:archived-run-create-github-issue-p
   #:archived-run-trunkp
   #:archived-run-periodic-job-p
   #:archived-run-screenshots
   #:archived-run-promotion-complete-p
   #:archived-run-override-commit-hash
   #:archived-run-compare-threshold
   #:archived-run-compare-tolerance
   #:archived-run-created-at
   #:archived-run-tags
   #:archived-run-batch
   #:archived-run-group-separator
   #:archived-run-was-promoted-p
   #:archived-run-author
   #:archived-run-metadata))
(in-package :screenshotbot/model/archived-run)


(defclass archived-run (base-indexed-object)
  ((channel
    :initarg :channel
    :initform nil
    :json-key "channel"
    :json-type (or null :string)
    :accessor archived-run-channel
    :documentation "The channel this run belongs to")
   (company
    :initarg :company
    :initform nil
    :json-key "company"
    :json-type (or null :string)
    :accessor archived-run-company
    :documentation "The company this run belongs to")
   (commit-hash
    :initarg :commit-hash
    :initform nil
    :json-key "commitHash"
    :json-type (or null :string)
    :accessor archived-run-commit)
   (build-url
    :initform nil
    :initarg :build-url
    :json-key "buildUrl"
    :json-type (or null :string)
    :accessor archived-run-build-url)
   (github-repo
    :initform nil
    :initarg :github-repo
    :json-key "githubRepo"
    :json-type (or null :string)
    :accessor archived-run-github-repo)
   (cleanp
    :initarg :cleanp
    :initform nil
    :json-key "cleanp"
    :json-type (or null :bool)
    :accessor archived-run-cleanp)
   (activep
    :initarg :activep
    :initform nil
    :json-key "activep"
    :json-type (or null :bool)
    :accessor archived-run-activep)
   (branch
    :initarg :branch
    :initform nil
    :json-key "branch"
    :json-type (or null :string)
    :accessor archived-run-branch
    :documentation "The main branch. This is not the branch associated with the
    current pull request!")
   (work-branch
    :initarg :work-branch
    :initform nil
    :json-key "workBranch"
    :json-type (or null :string)
    :accessor archived-run-work-branch
    :documentation "The branch we're currently working on, which might
    be the same as the main branch, or it might be the current pull
    request branch")
   (release-branch-p
    :initarg :release-branch-p
    :initform nil
    :json-key "releaseBranchP"
    :json-type (or null :bool)
    :accessor archived-run-release-branch-p
    :documentation "Whether the work-branch is a release branch")
   (branch-hash
    :initarg :branch-hash
    :initform nil
    :json-key "branchHash"
    :json-type (or null :string)
    :accessor archived-run-branch-hash
    :documentation "If a --branch is provided, this is the sha of the
    specified branch at the time of run. This might be different from
    the COMMIT-HASH, because the COMMIT-HASH might on, say a Pull
    Request (tied to the branch) or an ancestor of the branch.")
   (merge-base-hash
    :initform nil
    :initarg :merge-base
    :json-key "mergeBase"
    :json-type (or null :string)
    :accessor archived-run-merge-base
    :documentation "The merge base between branch-hash and commit-hash")
   (pull-request
    :initarg :pull-request
    :initform nil
    :json-key "pullRequest"
    :json-type (or null :string)
    :accessor archived-run-pull-request-url)
   (gitlab-merge-request-iid
    :initarg :gitlab-merge-request-iid
    :initform nil
    :json-key "gitlabMergeRequestIid"
    :json-type (or null :string)
    :accessor archived-run-gitlab-merge-request-iid)
   (phabricator-diff-id
    :initarg :phabricator-diff-id
    :initform nil
    :json-key "phabricatorDiffId"
    :json-type (or null :string)
    :accessor archived-run-phabricator-diff-id)
   (previous-run
    :initform nil
    :initarg :previous-run
    :json-key "previousRun"
    :json-type (or null :string)
    :accessor archived-run-previous-run
    :documentation "The previous *ACTIVE* run. Unpromoted runs aren't tracked on the channel, because often the reason it's unpromoted means that we don't understand if it belongs to the channel. If there are multile previous-runs for different branches, this will always point to the previous run on master branch.")
   (create-github-issue-p
    :initform nil
    :initarg :create-github-issue-p
    :json-key "createGithubIssueP"
    :json-type (or null :bool)
    :accessor archived-run-create-github-issue-p)
   (trunkp
    :initarg :trunkp
    :initform nil
    :json-key "trunkp"
    :json-type (or null :bool)
    :accessor archived-run-trunkp
    :documentation "whether this is tracking a production branch (as opposed to dev)")
   (periodic-job-p
    :initarg :periodic-job-p
    :initform nil
    :json-key "periodicJobP"
    :json-type (or null :bool)
    :accessor archived-run-periodic-job-p
    :documentation "Jobs that are done periodically, as opposed to for
    each commit. We will attempt to promote each run. This is mostly
    for taking screenshots of public websites.")
   (screenshots
    :initarg :screenshots
    :initform nil
    :json-key "screenshots"
    :json-type (or null :string)
    :accessor archived-run-screenshots
    :documentation "The list of screenshot names")
   (promotion-complete-p
    :initform nil
    :json-key "promotionCompleteP"
    :json-type (or null :bool)
    :accessor archived-run-promotion-complete-p)
   (override-commit-hash
    :initform nil
    :initarg :override-commit-hash
    :json-key "overrideCommitHash"
    :json-type (or null :string)
    :accessor archived-run-override-commit-hash
    :documentation "Override the pull request commit hash that will be
    used to update the Pull Request (either GitHub or Bitbucket)")
   (compare-threshold
    :initform nil
    :initarg :compare-threshold
    :json-key "compareThreshold"
    :json-type (or null :number)
    :accessor archived-run-compare-threshold
    :documentation "The comparison threshold in terms of fraction of pixels changed. If
NIL or 0, this will use exact pixel comparisons.")
   (compare-tolerance
    :initarg :compare-tolerance
    :initform nil
    :json-key "compareTolerance"
    :json-type (or null :number)
    :accessor archived-run-compare-tolerance)
   (created-at
    :initform nil
    :initarg :created-at
    :json-key "createdAt"
    :json-type (or null :number)
    :accessor archived-run-created-at)
   (tags
    :initarg :tags
    :initform nil
    :json-key "tags"
    :json-type (or null :string)
    :accessor archived-run-tags)
   (batch
    :initform nil
    :initarg :batch
    :json-key "batch"
    :json-type (or null :string)
    :accessor archived-run-batch
    :documentation "The batch object associated with this run")
   (group-separator
    :initarg :group-separator
    :initform nil
    :json-key "groupSeparator"
    :json-type (or null :string)
    :accessor archived-run-group-separator)
   (was-promoted-p
    :initarg :was-promoted-p
    :initform nil
    :json-key "wasPromotedP"
    :json-type (or null :bool)
    :accessor archived-run-was-promoted-p
    :documentation "Whether this was ever set as an active run for the run's channel,branch.")
   (author
    :initarg :author
    :initform nil
    :json-key "author"
    :json-type (or null :string)
    :accessor archived-run-author
    :documentation "The author, or owner of this run. This will be used for logic to ensure that authors cannot review their own runs. See T1056.")
   (metadata
    :initarg :metadata
    :initform nil
    :json-key "metadata"
    :json-type (or null :string)
    :accessor archived-run-metadata
    :documentation "An alist of metadata. The keys and values are both strings."))
  (:metaclass ext-json-serializable-class)
  (:documentation "An archived recorder run with minimal information, stored as JSON.
  This represents an old, finalized run that will be rarely directly referenced."))
