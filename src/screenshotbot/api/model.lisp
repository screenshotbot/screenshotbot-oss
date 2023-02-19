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
             #:override-commit-hash))

(in-package :screenshotbot/api/model)

;; Please update CHANGELOG.md
(defparameter *api-version* 4)

(defclass version ()
  ((version :initarg :version
            :json-key "version"
            :json-type :number
            :reader version-number))
  (:metaclass json-serializable-class))

(defmethod encode-json (object)
  (with-output-to-string (out)
    (json-mop:encode object out)))

(defmethod decode-json (json type)
  (json-mop:json-to-clos json type))

(defclass failed-run ()
  ((id :initarg :id
       :json-type :number
       :json-key "id")
   (channel :initarg :channel
            :json-key "channel"
            :json-type :string
            :reader failed-run-channel)
   (commit :initarg :commit
           :json-key "commit"
           :json-type :string
           :reader failed-run-commit))
  (:metaclass json-serializable-class))

(defclass screenshot ()
  ((name :initarg :name
         :json-type :string
         :json-key "name"
         :reader screenshot-name)
   (image-id :initarg :image-id
             :json-type :string
             :json-key "imageId"
             :reader screenshot-image-id)
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

(defclass run ()
  ((id :initarg :id
       :json-key "id"
       :json-type :string
       :reader run-id)
   (channel :initarg :channel
            :json-key "channel"
            :json-type :string
            :reader run-channel)
   (screenshots :initarg :screenshots
                :json-key "screenshots"
                :json-type (:list screenshot)
                :reader run-screenshots)
   (commit :initarg :commit-hash
           :json-key "commit"
           :json-type (or null :string)
           :reader run-commit)
   (create-github-issue :initarg :create-github-issue-p
                        :json-key "shouldCreateGithubIssue"
                        :json-type :boolea
                        :reader should-create-github-issue-p)
   (trunkp :initarg :trunkp
           :json-key "isTrunk"
           :json-type :boolean
           :reader trunkp)
   (periodic-job-p
    :initarg :periodic-job-p
    :json-key "isPeriodicJob"
    :json-type :boolean
    :reader periodic-job-p)
   (cleanp
    :initarg :cleanp
    :json-key "isClean"
    :json-type :boolean
    :reader cleanp)
   (pull-request-url :initarg :pull-request
                     :json-key "pullRequestUrl"
                     :json-type (or null :string)
                     :reader pull-request-url)
   (main-branch-hash :initarg :main-branch-hash
                     :json-key "mainBranchCommit"
                     :json-type (or null :string)
                     :reader main-branch-hash)
   (override-commit-hash :initarg :override-commit-hash
                         :json-key "overrideCommitHash"
                         :json-type (or null :string)
                         :reader override-commit-hash)
   (build-url :initarg :build-url
              :json-key "buildUrl"
              :json-type (or null :string)
              :reader build-url)
   (merge-base :initarg :merge-base
               :json-key "mergeBase"
               :json-type (or null :string)
               :reader merge-base)
   (main-branch :initarg :main-branch
                :json-key "mainBranch"
                :json-type (or null :string)
                :reader main-branch)
   (phabrictor-diff-id :initarg :phabricator-diff-id
                       :json-key "phabricatorDiff"
                       :json-type (or null :number)
                       :reader phabricator-diff-id)
   (gitlab-merge-request-iid :initarg :gitlab-merge-request-iid
                             :json-key "gitlabMergeRequestIID"
                             :json-type (or null :number)
                             :reader gitlab-merge-request-iid)
   (repo :initarg :github-repo
         :json-key "repo"
         :json-type (or null :string)
         ;; Internally this is github-repo :/
         :reader run-repo))
  (:metaclass ext-json-serializable-class))

(defmethod json-mop:json-to-clos ((items vector) (class (eql 'screenshot-list))
                                  &rest initargs)
  (loop for x across items
        collect (apply #'json-mop:json-to-clos x 'screenshot initargs)))
