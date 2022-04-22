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
                #:model-id
                #:recorder-run-screenshots
                #:recorder-run-channel
                #:recorder-previous-run
                #:pull-request-url
                #:recorder-run-commit
                #:activep)
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
   #:periodic-job-p))
(in-package :screenshotbot/model/recorder-run)

(defclass promotion-log (bknr.datastore:blob)
  ()
  (:metaclass persistent-class))

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
   (promotion-log
    :initform nil
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
    :initarg cleanp)
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
   (promotion-complete-p
    :initform nil
    :accessor promotion-complete-p)
   (created-at
    :initform nil
    :accessor %created-at))
  (:metaclass has-created-at))

(defun migrate-recorder-runs ()
  (flet ((listify (X)
           (if (listp x) x (list x))))
   (let ((all-runs (store-objects-with-class 'recorder-run)))
     (ensure-slot-boundp all-runs 'screenshots)
     (loop for run in all-runs
           do
              (let* ((screenshots (remove-if 'null (listify (recorder-run-screenshots run))))
                     (screenshots (mapcar 'screenshot-get-canonical screenshots)))
                (log:info "Checking: ~S" run)
                (unless (equal (recorder-run-screenshots run)
                               screenshots)
                  (log:info "updating screenshots for: ~S" run)
                  (with-transaction ()
                    (setf (recorder-run-screenshots run) screenshots))))))))


#+nil ;; needs screenshot.lisp to be loaded
(defun migrate-delete-unused-screenshots ()
  (let ((used (make-hash-table :test 'eql))
        (ctr 0))
    (loop for run in (store-objects-with-class 'recorder-run)
          do
             (let ((screenshots (recorder-run-screenshots run)))
               (loop for s in screenshots do
                 (setf (gethash s used) t))))
    (loop for s in (store-objects-with-class 'screenshotbot/model/screenshot:screenshot)
          do
             (progn
               (unless (gethash s used)
                 (log:info "Deleting (~a) ~S" ctr s)
                 (bknr.datastore:delete-object s)
                 (incf ctr))))
    ctr))

;; (migrate-recorder-runs)
;; (migrate-delete-unused-screenshots)

(defmethod model-id ((inst store-object))
  (store-object-id inst))

(let ((lock (bt:make-lock)))
  (defmethod promotion-log ((run recorder-run))
    (flet ((val () (%promotion-log run)))
     (or
      (val)
      (bt:with-lock-held (lock)
        (or
         (val)
         (with-transaction ()
           (setf (%promotion-log run)
                 (make-instance 'promotion-log)))))))))

;; (loop for run in (store-objects-with-class 'recorder-run) if (recorder-run-channel run) do (pushnew run (channel-runs (recorder-run-channel run))))
(let ((lock (bt:make-lock)))
 (defmethod bknr.datastore:initialize-transient-instance :after ((run recorder-run))
   (let ((channel (recorder-run-channel run)))
     (when channel
       (bt:with-lock-held (lock)
        (pushnew run (channel-runs channel)))))))

(defmethod initialize-instance :after ((run recorder-run) &key channel user previous-run activep &allow-other-keys)
  (declare (ignore previous-run user channel)))

(defmethod can-view ((run recorder-run) user)
  (can-view (recorder-run-channel run) user))

(defmethod can-public-view ((run recorder-run))
  (publicp (recorder-run-channel run)))

(defmethod activep ((run recorder-run))
  (let ((channel (recorder-run-channel run)))
   (eql run
        (active-run channel
                    (master-branch channel)))))

(defmethod (setf activep) (val (run recorder-run))
  (error "Old method, set active-run on channel directly"))
