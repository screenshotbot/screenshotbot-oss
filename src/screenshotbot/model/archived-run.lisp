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
  (:import-from #:screenshotbot/api/model
                #:metadata-key
                #:metadata-value
                #:metadata
                #:encode-json
                #:decode-json)
  (:import-from #:util/store/object-id
                #:find-by-oid
                #:oid
                #:oid-array)
  (:import-from #:util/store
                #:location-for-oid)
  (:import-from #:screenshotbot/model/recorder-run
                #:compare-tolerance
                #:recorder-run-metadata
                #:recorder-run-batch
                #:compare-threshold
                #:periodic-job-p
                #:trunkp
                #:run-build-url
                #:gitlab-merge-request-iid
                #:phabricator-diff-id
                #:recorder-run-merge-base
                #:recorder-run-author
                #:recorder-run-work-branch
                #:override-commit-hash
                #:github-repo
                #:recorder-run-branch
                #:recorder-run-warnings
                #:recorder-run-tags
                #:run-screenshot-map
                #:bknr-or-archived-run-mixin
                #:recorder-run-company)
  (:import-from #:screenshotbot/user-api
                #:activep
                #:pull-request-url
                #:recorder-run-commit
                #:%created-at
                #:recorder-run-channel)
  (:import-from #:bknr.datastore
                #:store-object-with-id)
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
   #:archived-run-metadata
   #:save-archived-run
   #:load-archived-run))
(in-package :screenshotbot/model/archived-run)


(defclass archived-run (base-indexed-object
                        bknr-or-archived-run-mixin)
  ((channel
    :initarg :channel
    :initform nil
    :json-key "channel"
    :json-type (or null :number)
    :accessor archived-run-channel
    :documentation "The channel this run belongs to")
   (oid
    :initarg :oid
    :initform nil
    :json-key "oid"
    :json-type (or null :string)
    :accessor %oid)
   (bknr-id
    :initarg :bknr-id
    :initform nil
    :json-key "bknrId"
    :json-type (or null :number)
    :documentation "The original bknr datastore id for this object")
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
    :accessor archived-run-commit
    :reader recorder-run-commit)
   (build-url
    :initform nil
    :initarg :build-url
    :json-key "buildUrl"
    :json-type (or null :string)
    :accessor archived-run-build-url
    :reader run-build-url)
   (github-repo
    :initform nil
    :initarg :github-repo
    :json-key "githubRepo"
    :json-type (or null :string)
    :accessor archived-run-github-repo
    :reader github-repo)
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
    :reader activep
    :accessor archived-run-activep)
   (branch
    :initarg :branch
    :initform nil
    :json-key "branch"
    :json-type (or null :string)
    :accessor archived-run-branch
    :reader recorder-run-branch
    :documentation "The main branch. This is not the branch associated with the
    current pull request!")
   (work-branch
    :initarg :work-branch
    :initform nil
    :json-key "workBranch"
    :json-type (or null :string)
    :accessor archived-run-work-branch
    :reader recorder-run-work-branch
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
    :reader recorder-run-merge-base
    :documentation "The merge base between branch-hash and commit-hash")
   (pull-request
    :initarg :pull-request
    :initform nil
    :json-key "pullRequest"
    :json-type (or null :string)
    :reader pull-request-url
    :accessor archived-run-pull-request-url)
   (gitlab-merge-request-iid
    :initarg :gitlab-merge-request-iid
    :initform nil
    :json-key "gitlabMergeRequestIid"
    :json-type (or null :string)
    :accessor archived-run-gitlab-merge-request-iid
    :reader gitlab-merge-request-iid)
   (phabricator-diff-id
    :initarg :phabricator-diff-id
    :initform nil
    :json-key "phabricatorDiffId"
    :json-type (or null :string)
    :reader phabricator-diff-id
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
    :reader trunkp
    :documentation "whether this is tracking a production branch (as opposed to dev)")
   (periodic-job-p
    :initarg :periodic-job-p
    :initform nil
    :json-key "periodicJobP"
    :json-type (or null :bool)
    :accessor archived-run-periodic-job-p
    :reader periodic-job-p
    :documentation "Jobs that are done periodically, as opposed to for
    each commit. We will attempt to promote each run. This is mostly
    for taking screenshots of public websites.")
   (screenshot-map
    :initarg :screenshots
    :initform nil
    :json-key "screenshotMap"
    :json-type (or null :number)
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
    :reader override-commit-hash
    :documentation "Override the pull request commit hash that will be
    used to update the Pull Request (either GitHub or Bitbucket)")
   (compare-threshold
    :initform nil
    :initarg :compare-threshold
    :json-key "compareThreshold"
    :json-type (or null :number)
    :accessor archived-run-compare-threshold
    :reader compare-threshold
    :documentation "The comparison threshold in terms of fraction of pixels changed. If
NIL or 0, this will use exact pixel comparisons.")
   (compare-tolerance
    :initarg :compare-tolerance
    :initform nil
    :json-key "compareTolerance"
    :json-type (or null :number)
    :accessor compare-tolerance
    :accessor archived-run-compare-tolerance)
   (created-at
    :initform nil
    :initarg :created-at
    :json-key "createdAt"
    :json-type (or null :number)
    :accessor archived-run-created-at
    :reader %created-at)
   (tags
    :initarg :tags
    :initform nil
    :json-key "tags"
    :json-type (or null :string)
    :accessor archived-run-tags
    :reader recorder-run-tags)
   (batch
    :initform nil
    :initarg :batch
    :json-key "batch"
    :json-type (or null :string)
    :accessor archived-run-batch
    :reader recorder-run-batch
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
    :reader recorder-run-author
    :documentation "The author, or owner of this run. This will be used for logic to ensure that authors cannot review their own runs. See T1056.")
   (metadata
    :initarg :metadata
    :initform nil
    :json-key "metadata"
    :json-type (:list metadata)
    :accessor archived-run-metadata
    :documentation "A list of metadata objects with key-value pairs."))
  (:metaclass ext-json-serializable-class)
  (:documentation "An archived recorder run with minimal information, stored as JSON.
  This represents an old, finalized run that will be rarely directly referenced."))

(defmethod oid ((run archived-run) &key (stringp t))
  "Returns the oid of the archived-run. If :STRINGP is T, then we return
the oid as a string. Otherwise we return the oid as stored."
  (let ((oid (%oid run)))
    (cond
      ((and stringp oid)
       oid)
      (t
       oid))))

(defun archived-run-location-for-oid (oid)
  "Figure out the local location for the given OID for an archived run"
  (location-for-oid
   #P"archived-runs/"
   oid
   :type "json"))

(defun save-archived-run (run)
  "Save an archived-run to disk as JSON using its OID to determine the location."
  (let* ((oid-str (oid run :stringp t))
         (oid-bytes (mongoid:oid oid-str))
         (file-path (archived-run-location-for-oid oid-bytes))
         (json-string (encode-json run)))
    (with-open-file (stream file-path
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
      (write-string json-string stream))
    file-path))

(defun load-archived-run (oid)
  "Load an archived-run from disk using its OID.
   OID can be a string, array, or OID struct."
  (let* ((oid-bytes (etypecase oid
                      (string (mongoid:oid oid))
                      ((vector (unsigned-byte 8)) oid)
                      (util/store/object-id::oid
                       (util/store/object-id::oid-arr oid))))
         (file-path (archived-run-location-for-oid oid-bytes)))
    (when (probe-file file-path)
     (with-open-file (stream file-path
                             :direction :input)
       (let ((json-string (with-output-to-string (out)
                            (loop for line = (read-line stream nil nil)
                                  while line
                                  do (write-line line out)))))
         (decode-json json-string 'archived-run))))))

(defmethod recorder-run-company ((self archived-run))
  (find-by-oid (archived-run-company self) '%c:company))

(defmethod recorder-run-channel ((self archived-run))
  (store-object-with-id (archived-run-channel self)))

(defmethod run-screenshot-map ((self archived-run))
  (store-object-with-id (archived-run-screenshots self)))


(defmethod recorder-run-warnings ((self archived-run))
  nil)

(defmethod recorder-run-metadata ((self archived-run))
  (loop for entry in (archived-run-metadata self)
        collect (cons
                 (metadata-key entry)
                 (metadata-value entry))))
