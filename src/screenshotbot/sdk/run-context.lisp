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
                #:when-let)
  (:import-from #:util/misc
                #:?.)
  (:import-from #:easy-macros
                #:def-easy-macro)
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
   #:author)
  (:local-nicknames (#:flags #:screenshotbot/sdk/flags)
                    (#:e #:screenshotbot/sdk/env)
                    (#:git #:screenshotbot/sdk/git)))
(in-package :screenshotbot/sdk/run-context)

(defclass run-context ()
  ((main-branch :initarg :main-branch
                :initform nil
                :reader main-branch)
   (main-branch-hash :initarg :main-branch-hash
                     :initform nil
                     :reader main-branch-hash)
   (pull-request-url :initarg :pull-request-url
                     :initform nil
                     :reader pull-request-url)
   (create-github-issue-p :initarg :create-github-issue-p
                          :initform nil
                          :reader create-github-issue-p)
   (repo-url :initarg :repo-url
             :initform nil
             :reader repo-url)
   (author :initarg :author
           :initform nil
           :reader author)
   (productionp :initarg :productionp
                :initform nil
                :reader productionp)
   (work-branch :initarg :work-branch
                :initform nil
                :reader work-branch)
   (build-url :initarg :build-url
              :initform nil
              :reader build-url)
   (gitlab-merge-request-iid :initarg :gitlab-merge-request-iid
                             :initform nil
                             :reader gitlab-merge-request-iid)
   (phabricator-diff-id :initarg :phabricator-diff-id
                        :initform nil
                        :reader phabricator-diff-id)
   (channel :initarg :channel
            :initform nil
            :reader channel)
   (override-commit-hash :initarg :override-commit-hash
                         :initform nil
                         :reader override-commit-hash)
   (commit-hash :initarg :commit-hash
                :initform nil
                :reader commit-hash)
   (merge-base :initarg :merge-base
               :initform nil
               :reader merge-base)
   (repo-clean-p :initarg :repo-clean-p
                 :reader repo-clean-p
                 :documentation "No initform!")
   (compare-threshold :initarg :compare-threshold
                      :initform nil
                      :reader compare-threshold)
   (batch :initarg :batch
          :initform nil
          :reader batch)
   (tags :initarg :tags
         :initform nil
         :reader tags)))

(defclass env-reader-run-context ()
  ((env :initarg :env
        :reader env)))

(defmethod git-repo ((self env-reader-run-context))
  (cond
    ((git-root :errorp nil)
     (make-instance 'git:git-repo))
    (t
     (log:warn "This is not running inside a Git repo. Please contact support@screenshotbot.io for advice, since the behavior in this case can be very different.")
     (make-instance 'git:null-repo))))

(defmethod build-url :around ((self env-reader-run-context))
  (or
   (call-next-method)
   (e:build-url (env self))))

(defmethod repo-url :around ((self env-reader-run-context))
  (or
   (call-next-method)
   (e:repo-url (env self))))

(defmethod work-branch :around ((self env-reader-run-context))
  (or
   (call-next-method)
   (e:work-branch (env self))))

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
   (when-let ((repo (git-repo self)))
     (git:merge-base repo (main-branch-hash self) (commit-hash self)))))

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

(defclass flags-run-context (env-reader-run-context
                             run-context)
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
   :override-commit-hash flags:*override-commit-hash*
   :author flags:*author*
   :compare-threshold flags:*compare-threshold*
   :batch flags:*batch*
   :tags (parse-tags flags:*tags*)))

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
        (flags:*batch* (batch self)))
    (fn)))
