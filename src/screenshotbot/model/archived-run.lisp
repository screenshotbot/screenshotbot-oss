;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(uiop:define-package :screenshotbot/model/archived-run
  (:use #:cl
        #:alexandria)
  (:import-from #:bknr.impex
                #:xml-class
                #:parse-xml-file
                #:write-to-xml)
  (:import-from #:bknr.indices
                #:base-indexed-object)
  (:export
   #:archived-run))
(in-package :screenshotbot/model/archived-run)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *archived-run-dtd*
    (asdf:system-relative-pathname :screenshotbot "dtd/archived-run.dtd")))

(defun serialize-boolean (value)
  "Serialize a boolean value to 'true' or 'false' string"
  (when value
    (if value "true" "false")))

(defun parse-boolean (value)
  "Parse a boolean value from string"
  (cond
    ((null value) nil)
    ((string-equal value "true") t)
    ((string-equal value "false") nil)
    ((string-equal value "t") t)
    ((string-equal value "nil") nil)
    (t nil)))

(defun parse-integer-or-nil (value)
  "Parse an integer value from string, or return nil"
  (when value
    (parse-integer value :junk-allowed nil)))

(defun parse-number-or-nil (value)
  "Parse a number (integer or float) from string, or return nil"
  (when value
    (let ((*read-eval* nil))
      (read-from-string value))))

(defclass archived-run (base-indexed-object)
  ((channel
    :initarg :channel
    :initform nil
    :attribute "channel"
    :accessor archived-run-channel
    :documentation "The channel this run belongs to")
   (company
    :initarg :company
    :initform nil
    :attribute "company"
    :accessor archived-run-company
    :documentation "The company this run belongs to")
   (commit-hash
    :initarg :commit-hash
    :initform nil
    :element "commit-hash"
    :accessor archived-run-commit)
   (build-url
    :initform nil
    :initarg :build-url
    :element "build-url"
    :accessor archived-run-build-url)
   (github-repo
    :initform nil
    :initarg :github-repo
    :element "github-repo"
    :accessor archived-run-github-repo)
   (cleanp
    :initarg :cleanp
    :initform nil
    :type boolean
    :attribute "cleanp"
    :parser 'parse-boolean
    :serializer 'serialize-boolean
    :accessor archived-run-cleanp)
   (activep
    :initarg :activep
    :initform nil
    :attribute "activep"
    :parser 'parse-boolean
    :serializer 'serialize-boolean
    :accessor archived-run-activep)
   (branch
    :initarg :branch
    :initform nil
    :element "branch"
    :accessor archived-run-branch
    :documentation "The main branch. This is not the branch associated with the
    current pull request!")
   (work-branch
    :initarg :work-branch
    :initform nil
    :element "work-branch"
    :accessor archived-run-work-branch
    :documentation "The branch we're currently working on, which might
    be the same as the main branch, or it might be the current pull
    request branch")
   (release-branch-p
    :initarg :release-branch-p
    :initform nil
    :attribute "release-branch-p"
    :parser 'parse-boolean
    :serializer 'serialize-boolean
    :accessor archived-run-release-branch-p
    :documentation "Whether the work-branch is a release branch")
   (branch-hash
    :initarg :branch-hash
    :initform nil
    :element "branch-hash"
    :accessor archived-run-branch-hash
    :documentation "If a --branch is provided, this is the sha of the
    specified branch at the time of run. This might be different from
    the COMMIT-HASH, because the COMMIT-HASH might on, say a Pull
    Request (tied to the branch) or an ancestor of the branch.")
   (merge-base-hash
    :initform nil
    :initarg :merge-base
    :element "merge-base"
    :accessor archived-run-merge-base
    :documentation "The merge base between branch-hash and commit-hash")
   (pull-request
    :initarg :pull-request
    :initform nil
    :element "pull-request"
    :accessor archived-run-pull-request-url)
   (gitlab-merge-request-iid
    :initarg :gitlab-merge-request-iid
    :initform nil
    :attribute "gitlab-merge-request-iid"
    :accessor archived-run-gitlab-merge-request-iid)
   (phabricator-diff-id
    :initarg :phabricator-diff-id
    :initform nil
    :attribute "phabricator-diff-id"
    :accessor archived-run-phabricator-diff-id)
   (previous-run
    :initform nil
    :initarg :previous-run
    :attribute "previous-run"
    :accessor archived-run-previous-run
    :documentation "The previous *ACTIVE* run. Unpromoted runs aren't tracked on the channel, because often the reason it's unpromoted means that we don't understand if it belongs to the channel. If there are multile previous-runs for different branches, this will always point to the previous run on master branch.")
   (create-github-issue-p
    :initform nil
    :initarg :create-github-issue-p
    :attribute "create-github-issue-p"
    :parser 'parse-boolean
    :serializer 'serialize-boolean
    :accessor archived-run-create-github-issue-p)
   (trunkp
    :initarg :trunkp
    :initform nil
    :attribute "trunkp"
    :parser 'parse-boolean
    :serializer 'serialize-boolean
    :accessor archived-run-trunkp
    :documentation "whether this is tracking a production branch (as opposed to dev)")
   (periodic-job-p
    :initarg :periodic-job-p
    :initform nil
    :attribute "periodic-job-p"
    :parser 'parse-boolean
    :serializer 'serialize-boolean
    :accessor archived-run-periodic-job-p
    :documentation "Jobs that are done periodically, as opposed to for
    each commit. We will attempt to promote each run. This is mostly
    for taking screenshots of public websites.")
   (screenshots
    :initarg :screenshots
    :initform nil
    :element "screenshots"
    :accessor archived-run-screenshots
    :documentation "The list of screenshot names")
   (promotion-complete-p
    :initform nil
    :attribute "promotion-complete-p"
    :parser 'parse-boolean
    :serializer 'serialize-boolean
    :accessor archived-run-promotion-complete-p)
   (override-commit-hash
    :initform nil
    :initarg :override-commit-hash
    :element "override-commit-hash"
    :accessor archived-run-override-commit-hash
    :documentation "Override the pull request commit hash that will be
    used to update the Pull Request (either GitHub or Bitbucket)")
   (compare-threshold
    :initform nil
    :initarg :compare-threshold
    :attribute "compare-threshold"
    :parser 'parse-number-or-nil
    :accessor archived-run-compare-threshold
    :documentation "The comparison threshold in terms of fraction of pixels changed. If
NIL or 0, this will use exact pixel comparisons.")
   (compare-tolerance
    :initarg :compare-tolerance
    :initform nil
    :attribute "compare-tolerance"
    :parser 'parse-number-or-nil
    :accessor archived-run-compare-tolerance)
   (created-at
    :initform nil
    :initarg :created-at
    :attribute "created-at"
    :parser 'parse-integer-or-nil
    :accessor archived-run-created-at)
   (tags
    :initarg :tags
    :initform nil
    :element "tags"
    :accessor archived-run-tags)
   (batch
    :initform nil
    :initarg :batch
    :attribute "batch"
    :accessor archived-run-batch
    :documentation "The batch object associated with this run")
   (group-separator
    :initarg :group-separator
    :initform nil
    :attribute "group-separator"
    :accessor archived-run-group-separator)
   (was-promoted-p
    :initarg :was-promoted-p
    :initform nil
    :attribute "was-promoted-p"
    :parser 'parse-boolean
    :serializer 'serialize-boolean
    :accessor archived-run-was-promoted-p
    :documentation "Whether this was ever set as an active run for the run's channel,branch.")
   (author
    :initarg :author
    :initform nil
    :element "author"
    :accessor archived-run-author
    :documentation "The author, or owner of this run. This will be used for logic to ensure that authors cannot review their own runs. See T1056.")
   (metadata
    :initarg :metadata
    :initform nil
    :element "metadata"
    :accessor archived-run-metadata
    :documentation "An alist of metadata. The keys and values are both strings."))
  (:metaclass xml-class)
  (:dtd-name *archived-run-dtd*)
  (:element "archived-run")
  (:documentation "An archived recorder run with minimal information, stored as XML.
  This represents an old, finalized run that will be rarely directly referenced."))
