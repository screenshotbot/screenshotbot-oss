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
  (:import-from #:bknr.datastore
                #:persistent-class
                #:with-transaction
                #:store-object-id
                #:store-object
                #:store-objects-with-class)
  (:import-from #:util
                #:object-with-oid)
  (:import-from #:screenshotbot/user-api
                #:user
                #:user-companies
                #:model-id
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
                #:when-let)
  (:import-from #:screenshotbot/model/core
                #:ensure-slot-boundp
                #:non-root-object)
  (:import-from #:screenshotbot/model/screenshot-map
                #:to-map
                #:make-set
                #:make-screenshot-map)
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
   #:not-fast-forward-promotion-warning
   #:run-screenshot-map
   #:make-recorder-run))
(in-package :screenshotbot/model/recorder-run)

(with-class-validation
 (defclass promotion-log (bknr.datastore:blob)
   ()
   (:metaclass persistent-class)))

(defclass transient-promotion-log ()
  ((oid-array :initarg :oid-array
              :reader oid-array)))

(defvar *lock* (bt:make-lock))

(with-class-validation
 (defclass recorder-run (object-with-oid)
   ((channel
     :initarg :channel
     :initform nil
     :relaxed-object-reference t
     :accessor recorder-run-channel)
    (company
     :initarg :company
     :reader recorder-run-company)
    (commit-hash
     :initarg :commit-hash
     :initform nil
     :accessor recorder-run-commit)
    #+screenshotbot-oss
    (promotion-log
     :accessor %promotion-log)
    (build-url
     :initform nil
     :initarg :build-url
     :accessor run-build-url)
    (github-repo
     :initform nil
     :initarg :github-repo
     :accessor github-repo)
    (cleanp
     :initarg :cleanp)
    (activep
     :initarg :activep
     :initform nil)
    (branch
     :initarg :branch
     :accessor recorder-run-branch)
    (branch-hash
     :initarg :branch-hash
     :initform nil
     :accessor recorder-run-branch-hash
     :documentation "If a --branch is provided, this is the sha of the
    specified branch at the time of run. This might be different from
    the COMMIT-HASH, because the COMMIT-HASH might on, say a Pull
    Request (tied to the branch) or an ancestor of the branch.")
    (merge-base-hash
     :initform nil
     :initarg :merge-base
     :accessor recorder-run-merge-base
     :documentation "The merge base between branch-hash and commit-hash")
    (pull-request
     :initarg :pull-request
     :initform nil
     :reader pull-request-url)
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
     :initform t
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
     :accessor recorder-run-screenshots)
    (%screenshot-map
     :initarg :screenshot-map
     :accessor run-screenshot-map)
    (promotion-complete-p
     :initform nil
     :accessor promotion-complete-p)
    (override-commit-hash
     :initform nil
     :initarg :override-commit-hash
     :accessor override-commit-hash
     :documentation "Override the pull request commit hash that will be
    used to update the Pull Request (either GitHub or Bitbucket)")
    (%compare-threshold
     :initform nil
     :initarg :compare-threshold
     :accessor compare-threshold
     :documentation "The comparison threshold in terms of fraction of pixels changed. If
NIL or 0, this will use exact pixel comparisons.")
    (%warnings
     :initform nil
     :accessor recorder-run-warnings
     :documentation "A list of warning objects that will be rendered whenever the run or an
associated report is rendered.")
    (created-at
     :initform nil
     :accessor %created-at))
   (:metaclass has-created-at)
   (:default-initargs :screenshot-map (error "need screenshot-map"))))

(defun make-recorder-run (&rest args &key screenshots channel &allow-other-keys)
  (apply #'make-instance 'recorder-run
         :screenshot-map (make-screenshot-map channel screenshots)
         args))

(defmethod print-object ((o recorder-run) stream)
  (format stream "#<RECORDER-RUN ~a>" (oid o)))

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

(defmethod model-id ((inst store-object))
  (store-object-id inst))

(defmethod promotion-log ((run recorder-run))
  (make-instance 'transient-promotion-log
                 :oid-array (oid-array run)))

;; (loop for run in (store-objects-with-class 'recorder-run) if (recorder-run-channel run) do (pushnew run (channel-runs (recorder-run-channel run))))
(defmethod bknr.datastore:initialize-transient-instance :after ((run recorder-run))
  (let ((channel (recorder-run-channel run)))
    (when channel
      (bt:with-lock-held (*lock*)
        (pushnew run (channel-runs channel))))))

(defmethod can-view ((run recorder-run) user)
  (can-view (recorder-run-channel run) user))

(defmethod can-public-view ((run recorder-run))
  (publicp (recorder-run-channel run)))

(defmethod can-edit ((run recorder-run) (user user))
  (member
   (recorder-run-company run)
   (user-companies user)))

(defmethod activep ((run recorder-run))
  (let ((channel (recorder-run-channel run)))
   (eql run
        (active-run channel
                    (master-branch channel)))))

(defmethod (setf activep) (val (run recorder-run))
  (error "Old method, set active-run on channel directly"))

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
                     (make-screenshot-map channel
                                          screenshots)))
               (log:info "Updating ~a" (bknr.datastore:store-object-id run))
               (with-transaction ()
                 (setf (run-screenshot-map run) map)))))))
    (ignore-this-run ()
      (values))
    (unset-screenshots-for-this-run ()
      (with-transaction ()
        (setf (recorder-run-screenshots run) nil)))))

(defmethod verify-screenshot-map (run)
  (log:info "Verifying ~a" run)
  (when (recorder-run-channel run)
   (let ((screenshots (recorder-run-screenshots run))
         (map (run-screenshot-map run)))
     (when (or screenshots map)
       (assert (fset:equal?
                (make-set screenshots)
                (to-map map)))))))

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

(with-class-validation
 (defclass merge-base-failed-warning (non-root-object)
   ((%compared-against :initarg :compared-against
                       :reader compared-against))
   (:metaclass persistent-class)
   (:documentation "A warning that when making a pull request comparison, we didn't
compare against the actual merge base.")))

(with-class-validation
  (defclass not-fast-forward-promotion-warning (non-root-object)
    ()
    (:metaclass persistent-class)
    (:documentation "A warning that this was not a fast-forward when the run was promoted")))
