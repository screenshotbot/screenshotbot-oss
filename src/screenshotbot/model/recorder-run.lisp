;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(uiop:define-package :screenshotbot/model/recorder-run
  (:use #:cl
        #:alexandria
        #:screenshotbot/model/core
        #:screenshotbot/model/view)
  (:nicknames #:%r)
  (:import-from #:bknr.datastore
                #:class-instances
                #:persistent-class
                #:with-transaction
                #:store-object-id
                #:store-object
                #:store-objects-with-class)
  (:import-from #:util
                #:object-with-oid)
  (:import-from #:screenshotbot/user-api
                #:user
                #:recorder-run-screenshots
                #:recorder-run-channel
                #:recorder-previous-run
                #:pull-request-url
                #:recorder-run-commit
                #:activep)
  (:import-from #:util/store
                #:location-for-oid
                #:with-class-validation)
  (:import-from #:util/object-id
                #:oid
                #:oid-array)
  (:import-from #:screenshotbot/model/view
                #:can-edit)
  (:import-from #:alexandria
                #:remove-from-plist
                #:when-let)
  (:import-from #:screenshotbot/model/core
                #:ensure-slot-boundp
                #:non-root-object)
  (:import-from #:util/store/store
                #:defindex)
  (:import-from #:util/store/fset-index
                #:fset-many-to-many-index
                #:fset-set-index)
  (:import-from #:util/store/store-migrations
                #:def-store-migration)
  (:import-from #:util/events
                #:push-event
                #:with-tracing)
  (:import-from #:util/misc
                #:?.)
  (:import-from #:auth/viewer-context
                #:viewer-context-api-key
                #:api-viewer-context
                #:viewer-context-user
                #:normal-viewer-context
                #:logged-in-viewer-context
                #:viewer-context)
  (:import-from #:util/store/simple-object-snapshot
                #:simple-object-snapshot)
  (:import-from #:core/api/model/api-key
                #:api-key-user
                #:cli-api-key
                #:api-key-permissions)
  (:import-from #:util/cron
                #:def-cron)
  (:import-from #:screenshotbot/model/constant-string
                #:ensure-slot-constant-string
                #:constant-string
                #:constant-string-string)
  (:import-from #:bknr.indices
                #:hash-index)
  ;; classes
  (:export #:promotion-log
           #:recorder-run)

  ;; methods
  (:export #:recorder-run-company
           #:recorder-run-merge-base
           #:phabricator-diff-id
           #:recorder-run-channel
           #:recorder-run-commit
           #:run-build-url
           #:github-repo
           #:activep
           #:screenshot-get-canonical ;; forward declaration
           #:cleanp
           #:promotion-complete-p
           #:recorder-previous-run
           #:finalize-promotion
           #:trunkp
           #:channel-runs
           #:push-run-to-channel
           #:promotion-log
           #:publicp
           #:active-run
           #:gitlab-merge-request-iid
           #:pull-request-url
           #:recorder-run-branch
           #:recorder-run-branch-hash
           #:recorder-run-commit
           #:create-github-issue-p
           #:recorder-run-screenshots
           #:master-branch)
  (:export
   #:periodic-job-p
   #:override-commit-hash
   #:unpromote-run
   #:pull-request-id
   #:compared-against
   #:compare-threshold
   #:compare-tolerance
   #:not-fast-forward-promotion-warning
   #:run-screenshot-map
   #:make-recorder-run
   #:remove-run-from-channel
   #:runs-for-company
   #:recorder-run-work-branch
   #:recorder-run-batch
   #:recorder-run-repo-url
   #:group-separator
   #:recorder-run-author
   #:abstract-run
   #:delete-run
   #:was-promoted-p
   #:find-shards
   #:shard-number
   #:shard-count
   #:shard-screenshots
   #:shard-key
   #:recorder-run-metadata
   #:recorder-run-uname)
  (:local-nicknames (#:screenshot-map #:screenshotbot/model/screenshot-map)))
(in-package :screenshotbot/model/recorder-run)

(with-class-validation
 (defclass promotion-log (bknr.datastore:blob)
   ()
   (:metaclass persistent-class)))

(defclass transient-promotion-log ()
  ((oid-array :initarg :oid-array
              :reader oid-array)))

(defvar *lock* (bt:make-lock))

(defvar *warning-lock* (bt:make-lock))

(defindex +run-company-index+
  'fset-set-index
  :slot-name 'company)

(defindex +tags-index-v2+
  'fset-many-to-many-index
  :slot-name 'tags)

(defindex +run-channel-index+
  'fset-set-index
  :slot-name 'channel)

(defclass bknr-or-archived-run-mixin ()
  ()
  (:documentation "A class that's mixed into both RECORDER-RUN and ARCHIVED-RUN, so that
we can write methods that are generic to both."))

(defclass abstract-run (store-object)
  ()
  (:metaclass persistent-class))

(defindex +shard-key-index+
  'fset-set-index
  :slot-name '%key)

(with-class-validation
  (defclass shard (store-object)
    ((%screenshots :initarg :screenshots
                   :reader shard-screenshots)
     (%company :initarg :company
               :reader shard-company)
     (%key :initarg :key
           :reader shard-key
           :index +shard-key-index+
           :index-reader %shards-for-key)
     (%number :initarg :number
              :reader shard-number)
     (%count :initarg :count
             :reader shard-count)
     (%channel :initarg :channel
               :reader shard-channel)
     (%ts :initarg :ts
          :reader shard-ts))
    (:metaclass persistent-class)
    (:default-initargs :ts (get-universal-time)
                       :channel nil)))

(defun find-shards (channel key)
  (reverse ;; most recent first
   (loop for shard in (fset:convert 'list (%shards-for-key key))
         if (and
             (eql channel (shard-channel shard)))
           collect shard)))

(with-class-validation
  (defclass recorder-run (object-with-oid abstract-run
                          bknr-or-archived-run-mixin)
    ((channel
      :initarg :channel
      :initform nil
      :relaxed-object-reference t
      :index +run-channel-index+
      :index-reader runs-for-channel
      :accessor recorder-run-channel)
     (company
      :initarg :company

      
      :initform nil
      :accessor recorder-run-company
      :index +run-company-index+
      :index-reader runs-for-company
      :documentation "Heads up! This can be NIL for deleted runs, which might be referenced from non-deleted reports (T1876)")
     (commit-hash
      :initarg :commit-hash
      :initform nil
      :accessor %recorder-run-commit)
     #+screenshotbot-oss
     (promotion-log
      :accessor %promotion-log)
     (build-url
      :initform nil
      :initarg :build-url
      :accessor %run-build-url)
     (github-repo
      :initform nil
      :initarg :github-repo
      :accessor github-repo
      :accessor recorder-run-repo-url)
     (cleanp
      :initarg :cleanp)
     (activep
      :initarg :activep
      :initform nil)
     (branch
      :initarg :branch
      :initform nil
      :accessor recorder-run-branch
      :documentation "The main branch. This is not the branch associated with the
    current pull request!")
     (work-branch
      :initarg :work-branch
      :initform nil
      :accessor recorder-run-work-branch
      :documentation "The branch we're currently working on, which might
    be the same as the main branch, or it might be the current pull
    request branch")
     (release-branch-p
      :initarg :release-branch-p
      :accessor release-branch-p
      :documentation "Whether the work-branch is a release branch")
     (branch-hash
      :initarg :branch-hash
      :initform nil
      :accessor %recorder-run-branch-hash
      :documentation "If a --branch is provided, this is the sha of the
    specified branch at the time of run. This might be different from
    the COMMIT-HASH, because the COMMIT-HASH might on, say a Pull
    Request (tied to the branch) or an ancestor of the branch.")
     (merge-base-hash
      :initform nil
      :initarg :merge-base
      :accessor %recorder-run-merge-base
      :documentation "The merge base between branch-hash and commit-hash")
     (pull-request
      :initarg :pull-request
      :initform nil
      :accessor %pull-request-url)
     (gitlab-merge-request-iid
      :initarg :gitlab-merge-request-iid
      :initform nil
      :reader gitlab-merge-request-iid)
     (phabricator-diff-id
      :initarg :phabricator-diff-id
      :initform nil
      :reader phabricator-diff-id)
     (previous-run
      :initform nil
      :initarg :previous-run
      :accessor recorder-previous-run
      :documentation "The previous *ACTIVE* run. Unpromoted runs aren't tracked on the channel, because often the reason it's unpromoted means that we don't understand if it belongs to the channel. If there are multile previous-runs for different branches, this will always point to the previous run on master branch.")
     (create-github-issue-p
      :initform nil
      :initarg :create-github-issue-p
      :accessor create-github-issue-p)
     (trunkp
      :initarg :trunkp
      :accessor trunkp
      :initform nil
      :documentation "whether this is tracking a production branch (as opposed to dev)")
     (periodic-job-p
      :initarg :periodic-job-p
      :initform nil
      :accessor periodic-job-p
      :documentation "Jobs that are done periodically, as opposed to for
    each commit. We will attempt to promote each run. This is mostly
    for taking screenshots of public websites.")
     (Screenshots
      :initarg :screenshots
      :initform nil
      :documentation "The old list of screenshots, these days we generate them from the map")
     (%screenshot-map
      :initarg :screenshot-map
      :accessor run-screenshot-map)
     (promotion-complete-p
      :initform nil
      :accessor promotion-complete-p)
     (override-commit-hash
      :initform nil
      :initarg :override-commit-hash
      :accessor %override-commit-hash
      :documentation "Override the pull request commit hash that will be
    used to update the Pull Request (either GitHub or Bitbucket)")
     (%compare-threshold
      :initform nil
      :initarg :compare-threshold
      :accessor compare-threshold
      :documentation "The comparison threshold in terms of fraction of pixels changed. If
NIL or 0, this will use exact pixel comparisons.")
     (%compare-tolerance
      :initarg :compare-tolerance
      :accessor compare-tolerance)
     (%warnings
      :initform nil
      :accessor recorder-run-warnings
      :documentation "A list of warning objects that will be rendered whenever the run or an
associated report is rendered.")
     (created-at
      :initform nil
      :accessor %created-at)
     (tags
      :initarg :tags
      :accessor recorder-run-tags
      :index +tags-index-v2+
      :index-reader %runs-for-tag)
     (batch :initform nil
            :initarg :batch
            :accessor recorder-run-batch
            :documentation "The batch object associated with this run")
     (%group-separator :initarg :group-separator
                       :reader group-separator)
     (%was-promoted-p :initarg :was-promoted-p
                      :accessor was-promoted-p
                      :documentation "Whether this was ever set as an active run for the run's channel,branch.")
     (%author :initarg :author
              :reader recorder-run-author
              :documentation "The author, or owner of this run. This will be used for logic to ensure that authors cannot review their own runs. See T1056.")
     (%metadata :initarg :metadata
                :reader recorder-run-metadata
                :documentation "An alist of metadata. The keys and values are both strings."))
    (:metaclass has-created-at)
    (:default-initargs :screenshot-map (error "need screenshot-map")
                       :compare-tolerance nil
                       :release-branch-p nil
                       :author nil
                       :metadata nil
                       :tags nil
                       :was-promoted-p nil)))

(defun recorder-run-uname (run)
  (assoc-value (recorder-run-metadata run)
               "uname" :test #'equal))

(defmethod pull-request-url ((self recorder-run))
  (constant-string-string (%pull-request-url self)))

(defmethod run-build-url ((self recorder-run))
  (constant-string-string (%run-build-url self)))

(defmethod recorder-run-commit ((self recorder-run))
  (constant-string-string (%recorder-run-commit self)))

(defmethod (setf recorder-run-commit) (val (self recorder-run))
  (setf (%recorder-run-commit self)
        (constant-string val)))

(defmethod recorder-run-branch-hash ((self recorder-run))
  (constant-string-string (%recorder-run-branch-hash self)))

(defmethod recorder-run-merge-base ((self recorder-run))
  (constant-string-string (%recorder-run-merge-base self)))

(defmethod override-commit-hash ((self recorder-run))
  (constant-string-string (%override-commit-hash self)))

(defmethod bknr.datastore:make-object-snapshot ((self recorder-run))
  (make-instance 'simple-object-snapshot
                 :object self
                 :except-slots '(promotion-complete-p
                                 %warnings)))

(defmethod recorder-run-author :around ((run recorder-run))
  (handler-case
      (call-next-method)
    (unbound-slot (e)
      nil)))

(defun assert-no-loops (recorder-run)
  (flet ((next (run)
           (when run
            (recorder-previous-run run))))
   (when recorder-run
     (labels ((work (slow fast)
                (cond
                  ((not fast)
                   ;; there's no loop here.
                   t)
                  ((eql slow fast)
                   (error "There's a loop in this run history at ~a" slow))
                  (t
                   (work (next slow)
                         (next (next fast)))))))
       (work recorder-run (next recorder-run))))))

(defindex +unchanged-run-index+
  'hash-index
  :test #'equal
  :slot-name 'commit)

(defmethod recorder-run-commit :around (run)
  (or
   (call-next-method)
   (override-commit-hash run)))

(with-class-validation
 (defclass unchanged-run (abstract-run)
   ((commit :initarg :commit
            :reader unchanged-run-commit
            :index +unchanged-run-index+
            :index-reader %unchanged-runs-for-commit
            :reader recorder-run-commit)
    (other-commit :initarg :other-commit
                  :reader unchanged-run-other-commit
                  :documentation "The commit that this is going to be a copy of")
    (channel :initarg :channel
             :initform nil
             :reader unchanged-run-channel
             :reader recorder-run-channel)
    (%merge-base :initarg :merge-base
                 :reader recorder-run-merge-base)
    (%override-commit-hash :initarg :override-commit-hash
                           :reader override-commit-hash)
    (%work-branch :initarg :work-branch
                  :reader recorder-run-work-branch)
    (%batch :initarg :batch
            :accessor recorder-run-batch
            :documentation "The batch object associated with this run")
    (%%created-at :initarg :created-at
                 :accessor %created-at)
    (promotion-complete-p
     :initform nil
     :accessor promotion-complete-p))
   (:metaclass persistent-class)
   (:default-initargs :batch nil
                      :override-commit-hash nil
                      :created-at (get-universal-time)
                      :merge-base nil)
   (:documentation "Annotates that this commit should have identical screenshots to the other commit")))

(defun unchanged-run-for-commit (channel commit)
  (let ((runs
          ;; Backward compatibility for a migration from
          ;; fset-set-index to hash-index.
          (fset:convert 'fset:set
                        (%unchanged-runs-for-commit commit))))
    (fset:do-set (ur runs)
      (when (eql channel (unchanged-run-channel ur))
        (return-from unchanged-run-for-commit ur))))
  nil)

(defmethod bknr.datastore:make-object-snapshot ((self unchanged-run))
  (make-instance 'simple-object-snapshot
                 :object self
                 :except-slots '(promotion-complete-p)))

(defun make-recorder-run (&rest args &key screenshots channel
                                       pull-request
                                       build-url
                                       commit-hash
                                       branch-hash
                                       merge-base
                                       override-commit-hash
                          &allow-other-keys)
  (apply #'make-instance 'recorder-run
         :pull-request (constant-string pull-request)
         :build-url (constant-string build-url)
         :commit-hash (constant-string commit-hash)
         :branch-hash (constant-string branch-hash)
         :merge-base (constant-string merge-base)
         :override-commit-hash (constant-string override-commit-hash)
         :screenshot-map (screenshot-map:make-screenshot-map channel screenshots)
         (remove-from-plist args :screenshots)))

(defmethod print-object ((o recorder-run) stream)
  (format stream "#<RECORDER-RUN ~a>" (ignore-errors (oid o))))

(defmethod pull-request-id (run)
  (when-let ((url (pull-request-url run)))
    (when-let ((part (last (str:split "/" url))))
      (ignore-errors
       (parse-integer (car part))))))

(defun unpromote-run (run)
  (let ((previous-run (recorder-previous-run run))
        (channel (recorder-run-channel run))
        (branch (recorder-run-branch run)))
    (assert (eql run (active-run channel branch)))
    (with-transaction ()
     (setf (recorder-previous-run run) nil))
    (setf (active-run channel branch) previous-run)))

(defmethod promotion-log ((run recorder-run))
  (make-instance 'transient-promotion-log
                 :oid-array (oid-array run)))

(defmethod bknr.datastore:initialize-transient-instance :after ((run recorder-run))
  (let ((channel (recorder-run-channel run)))
    (when channel
      (push-run-to-channel channel run))))

(defmethod bknr.datastore::destroy-object :before ((run recorder-run))
  (when-let ((channel (recorder-run-channel run)))
    (unless (bknr.datastore::object-destroyed-p channel)
      (remove-run-from-channel channel run))))

(defmethod auth:can-view ((run bknr-or-archived-run-mixin) user)
  (auth:can-view-with-normal-viewer-context
   user run))

(defmethod auth:can-view ((run recorder-run) user)
  ;; TODO: delete
  (call-next-method))

(defmethod auth:can-viewer-view (vc (run bknr-or-archived-run-mixin))
  (or
   (publicp (recorder-run-channel run))
   (auth:can-viewer-view vc (recorder-run-channel run))))

(defmethod auth:can-viewer-view (vc (run recorder-run))
  ;; TODO: delete
  (call-next-method))

(defmethod can-api-key-view-run-p (api-key run)
  (member :full (api-key-permissions api-key)))

(defmethod can-api-key-view-run-p ((api-key cli-api-key) run)
  "from a cli-api-key, we need to be able to access our runs... but we
might also want to build a feature to check against our current main
branch runs."
  t)

(defmethod auth:can-viewer-view :around ((vc api-viewer-context) (run recorder-run))
  (and
   (or
    (not (gk:check :api-key-roles (recorder-run-company run)))
    (can-api-key-view-run-p (viewer-context-api-key vc) run))
   (call-next-method)))

(defmethod can-public-view ((run recorder-run))
  (publicp (recorder-run-channel run)))

(defmethod auth:can-edit ((run recorder-run) (user user))
  (auth:can-edit-with-normal-viewer-context
   user run))

(defmethod auth:can-viewer-edit ((vc normal-viewer-context) (run recorder-run))
  (roles:has-role-p
   (recorder-run-company run)
   (viewer-context-user vc)
   'roles:standard-member))

(defmethod activep ((run recorder-run))
  (let ((channel (recorder-run-channel run)))
   (eql run
        (active-run channel
                    (master-branch channel)))))

(defmethod (setf activep) (val (run recorder-run))
  (error "Old method, set active-run on channel directly"))

(defmethod recorder-run-screenshots ((run bknr-or-archived-run-mixin))
  (screenshot-map:to-list
   (run-screenshot-map run)))


(defmethod bknr.datastore:blob-pathname ((self transient-promotion-log))
  (ensure-directories-exist
   (location-for-oid #P"promotion-logs/"
                     (oid-array self))))

(defmethod ensure-screenshot-map ((run recorder-run))
  (restart-case
      (when-let ((channel (recorder-run-channel run)))
        (unless (run-screenshot-map run)
          (let ((screenshots (recorder-run-screenshots run)))
            (when (listp screenshots)
             (let ((map
                     (screenshot-map:make-screenshot-map channel
                                                         screenshots)))
               (log:info "Updating ~a" (bknr.datastore:store-object-id run))
               (with-transaction ()
                 (setf (run-screenshot-map run) map)))))))
    (ignore-this-run ()
      (values))))

(defmethod verify-screenshot-map (run)
  (log:info "Verifying ~a" run)
  (when (recorder-run-channel run)
   (let ((screenshots (recorder-run-screenshots run))
         (map (run-screenshot-map run)))
     (when (or screenshots map)
       (let ((screenshot-set (screenshot-map:make-set screenshots)))
         (assert (fset:equal?
                  screenshot-set
                  (screenshot-map:to-map map)))
         (loop for x in (fset:convert 'list screenshot-set)
               for y in (fset:convert 'list (screenshot-map:to-map map))
               do (assert (eql (car x) (car y)))))))))

;; (mapc #'verify-screenshot-map (bknr.datastore:class-instances 'recorder-run))

(defun ensure-all-screenshot-maps ()
  (ensure-slot-boundp 'recorder-run '%screenshot-map)
  (time
   (mapc #'ensure-screenshot-map
         (bknr.datastore:class-instances 'recorder-run))))

;; Migration
#+screenshotbot-oss
(defun convert-old-promotion-logs ()
  (loop for run in (bknr.datastore:store-objects-with-class 'recorder-run)
        for promotion-log = (%promotion-log run)
        if (typep promotion-log 'promotion-log)
          do
             (let ((pathname (bknr.datastore:blob-pathname promotion-log)))
              (restart-case
                  (progn
                    (with-transaction ()
                      (setf (%promotion-log run) nil))
                    (when (path:-e pathname)
                      (rename-file
                       pathname
                       (bknr.datastore:blob-pathname (promotion-log run))))
                    (bknr.datastore:delete-object promotion-log))
                (ignore-this-blob ()
                  (values))))))

(defindex +run-warning-run-index+
  'hash-index
  :slot-name '%run)

(defclass base-run-warning (store-object)
  ((%run :initarg :run
         :index +run-warning-run-index+
         :index-reader warnings-for-run))
  (:metaclass persistent-class))

(with-class-validation
 (defclass merge-base-failed-warning (base-run-warning)
   ((%compared-against :initarg :compared-against
                       :reader compared-against))
   (:metaclass persistent-class)
   (:documentation "A warning that when making a pull request comparison, we didn't
compare against the actual merge base.")))

(with-class-validation
  (defclass not-fast-forward-promotion-warning (base-run-warning)
    ()
    (:metaclass persistent-class)
    (:documentation "A warning that this was not a fast-forward when the run was promoted")))

(def-store-migration ("Add compare tolerance" :version 7)
  (ensure-slot-boundp 'recorder-run '%compare-tolerance))

(def-store-migration ("Add tags to model" :version 8)
  (ensure-slot-boundp 'recorder-run 'tags))

(defmethod runs-for-tag (company tag)
  (let ((runs (fset:convert 'list (%runs-for-tag tag))))
    (loop for run in runs
          if (eql company (recorder-run-company run))
            collect run)))

(defmethod group-separator :around ((run recorder-run))
  (or (ignore-errors (call-next-method))
      "--"))

(defmethod recorder-run-company ((self unchanged-run))
  (screenshotbot/model/company:company (unchanged-run-channel self)))

(def-store-migration ("Ensure :company slot is set to the channel company" :version 17)
  (ensure-slot-boundp 'recorder-run 'company)
  (dolist (run (bknr.datastore:class-instances 'recorder-run))
    (when (and
           (not (recorder-run-company run))
           (recorder-run-channel run)
           (ignore-errors
            (screenshotbot/model/company:company (recorder-run-channel run))))
      (format t "Fixing run: ~a~%" run)
      (setf (recorder-run-company run)
            (screenshotbot/model/company:company (recorder-run-channel run) )))))

(defmethod delete-run (run)
  "Not exactly deleting, but currently just detaching it from the
company as a way of deleting."
  (push-event :delete-run :run (oid run) :company (?. oid (recorder-run-company run)))
  (setf (recorder-run-company run) nil))

(def-store-migration ("Ensure :was-promoted-p is bound" :Version 25)
  (ensure-slot-boundp 'recorder-run '%was-promoted-p))

(defun clean-up-old-shards ()
  (let ((cut-off (- (get-universal-time) (* 24 3600 2))))
    (loop for shard in (class-instances 'shard)
          if (< (shard-ts shard) cut-off)
          do (bknr.datastore:delete-object shard))))

(def-cron clean-up-old-shards (:minute 0 :hour 1)
  (clean-up-old-shards))

(def-store-migration ("Use constant-string for pull-request" :version 26)
  (ensure-slot-constant-string
   (bknr.datastore:class-instances 'recorder-run)
   'pull-request))

(def-store-migration ("Use constant-string for build-url" :version 27)
  (ensure-slot-constant-string
   (bknr.datastore:class-instances 'recorder-run)
   'build-url))

(def-store-migration ("Use constant-string for commits on run" :version 28)
  (loop for slot in '(commit-hash
                      branch-hash
                      merge-base-hash
                      override-commit-hash)
        do
           (ensure-slot-boundp 'recorder-run slot)
           (ensure-slot-constant-string
            (bknr.datastore:class-instances 'recorder-run)
            slot)))

(def-store-migration ("Add new slot for metadata" :version 34)
  "This migration was supposed to be for version 29, but because of a
typo it wasn't being run. I've updated the version."
  (ensure-slot-boundp 'recorder-run '%metadata))

(def-store-migration ("Add new slot for release-branch-p" :version 30)
  (ensure-slot-boundp 'recorder-run 'release-branch-p))

(defun push-run-warning (run type &rest args)
  "Create a run warning of type TYPE with args ARGS, and push it to the
list of warnings for RUN."
  (let ((warning (apply #'make-instance type
                        :run run
                        args)))
    (bt:with-lock-held (*warning-lock*)
      (push warning (recorder-run-warnings run)))))

(defun delete-old-unchanged-runs (&key (now (get-universal-time)))
  (let ((cutoff (- now (* 3600 24 180))))
    (loop for unchanged-run in (bknr.datastore:class-instances 'unchanged-run)
          if (<
              (handler-case
                  (%created-at unchanged-run)
                (unbound-slot ()
                  0))
              cutoff)
            do
               (bknr.datastore:delete-object unchanged-run))))

(def-cron delete-old-unchanged-runs (:minute 22 :hour 4)
  (delete-old-unchanged-runs))

(def-store-migration ("Backfill run slot on warnings" :version 37)
  (dolist (run (bknr.datastore:class-instances 'recorder-run))
    (dolist (warning (recorder-run-warnings run))
      (when (typep warning 'base-run-warning)
        (unless (slot-boundp warning '%run)
          (setf (slot-value warning '%run) run))))))
