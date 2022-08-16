;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(uiop:define-package :screenshotbot/model/channel
  (:use #:cl
        #:alexandria
        #:screenshotbot/model/core
        #:screenshotbot/user-api
        #:screenshotbot/model/view)
  (:import-from #:screenshotbot/git-repo
                #:generic-git-repo)
  (:import-from #:screenshotbot/plugin
                #:plugin-parse-repo)
  (:import-from #:screenshotbot/installation
                #:plugins
                #:installation)
  (:import-from #:bknr.datastore
                #:persistent-class
                #:store-object
                #:store-objects-with-class
                #:store-object-id
                #:hash-index
                #:deftransaction)
  (:import-from #:screenshotbot/model/image
                #:masks)
  (:import-from #:screenshotbot/model/recorder-run
                #:recorder-run
                #:recorder-run-commit
                #:master-branch
                #:active-run
                #:recorder-previous-run
                #:channel-runs
                #:github-repo
                #:activep
                #:publicp
                #:recorder-run-channel)
  (:import-from #:screenshotbot/model/company
                #:company
                #:company-runs)
  (:import-from #:util/store
                #:with-class-validation)
  (:export
   #:channel
   #:set-channel-screenshot-mask
   #:channel-branch
   #:channel-promoted-runs
   #:channel-name
   #:channel-runs
   #:channel-repo
   #:channel-promotion-lock
   #:production-run-for
   #:*channel-repo-overrides*
   #:masks
   #:channel-cv
   #:channel-lock
   #:all-active-runs)

  ;; forward decls
  (:export
   #:repo-left-ancestor-p
   #:make-gitlab-repo
   #:github-get-canonical-repo))
(in-package :screenshotbot/model/channel)


;; hack. While promoting we want to temporarily give the channel a new
;; repo link
(defvar *channel-repo-overrides* nil)

(with-class-validation
 (defclass channel (store-object)
   ((name :initarg :name
          :reader channel-name)
    (company
     :initarg :company
     :initform nil
     :accessor company)
    (active-runs
     :initform nil
     :accessor all-active-runs
     :relaxed-object-reference t
     :documentation "Mapping from branch name to active run as an alist")
    (branch :initarg :branch
            :accessor channel-branch)
    (github-repo :initarg :github-repo
                 :writer (setf github-repo)
                 :reader %%github-repo)
    (github-repo-full-name :accessor github-repo-full-name
                           :index-type hash-index
                           :index-initargs (:test 'equal)
                           :index-reader channel-with-github-repo-full-name)
    (master-branch :accessor master-branch
                   :initform "master")
    (masks :initform nil
           :reader masks
           :documentation "assoc value from screenshot name to list of MASK-RECTs")
    (publicp :initform nil
             :initarg :publicp
             :accessor publicp)
    (repo-cache
     :initform nil
     :transient t)
    (channel-runs
     :initform nil
     :transient t
     :accessor channel-runs)
    (lock
     :initform (bt:make-lock)
     :transient t
     :reader channel-lock)
    (promotion-lock
     :initform (bt:make-lock)
     :transient t
     :reader channel-promotion-lock)
    (cv
     :initform (bt:make-condition-variable :name "channel-cv")
     :transient t
     :reader channel-cv)
    (created-at
     :initform (local-time:timestamp-to-universal (local-time:now))
     :accessor %created-at))
   (:metaclass persistent-class)))

(defmethod channel-company ((channel channel))
  (company channel))

(defun all-channels ()
  (store-objects-with-class 'channel))

(defmethod github-repo ((channel channel))
  (or
   (assoc-value *channel-repo-overrides* channel)
   (%%github-repo channel)))

(defmacro with-channel-lock ((channel) &body body)
  `(flet ((body () ,@body))
     (with-slots (lock) ,channel
       (bt:with-lock-held (lock)
         (body)))))

(defmethod (setf github-repo) :after (val (channel channel))
  (setf (github-repo-full-name channel)
        (get-full-repo-from-repo val)))

(defun get-full-repo-from-repo (repo)
  (when repo
   (multiple-value-bind (full parts)
       (cl-ppcre:scan-to-strings
        "^https://github.com/(.*/.*)$"
        (github-get-canonical-repo repo))
     (when full
       (elt parts 0)))))


(deftransaction
    %set-active-run (channel branch run)
    (check-type run recorder-run)
    (check-type channel channel)
    (check-type branch string)
    (setf
     (assoc-value (all-active-runs channel)
                  branch
                  :test 'equal)
     run))

(deftransaction
    set-channel-screenshot-mask (channel name mask)
    (check-type channel channel)
    (check-type name string)
    (check-type mask list)
    (setf (assoc-value (slot-value channel 'masks) name :test 'equal)
          mask))

(defmethod (setf active-run) ((run recorder-run) (channel channel) (branch string))
  (%set-active-run channel branch run))

(defmethod active-run ((channel channel) branch)
  (assoc-value (all-active-runs channel)
               branch :test 'equal))

(defmethod clear-caches ((channel channel))
  (with-slots (repo-cache) channel
    (setf repo-cache nil)))

(defmethod channel-repo ((channel channel))
  (loop for plugin in (plugins (installation))
        for repo = (plugin-parse-repo plugin
                                      (channel-company channel)
                                      (github-repo channel))
        if repo
          return repo
        finally
           (return (make-instance 'generic-git-repo :link (github-repo channel)
                                             :company (channel-company channel)))))

(defmethod channel-left-ancestor-in-branch-p ((channel channel) commit)
  (declare (optimize (speed 0) (debug 3)))
  (when (master-branch channel)
   (repo-left-ancestor-p
    (channel-repo channel)
    commit
    (master-branch channel))))

(defmethod created-at (x)
  (local-time:universal-to-timestamp (%created-at x)))

(defmethod can-view ((channel channel) user)
  (or
   (publicp channel)
   (is-user-id-same channel user)))

(defmethod production-run-for ((channel channel)
                               &key commit)
  ;; currently returns the oldest run for a given commit
  (let ((large-int most-positive-fixnum))
   (reduce
    (lambda (x y)
      (if (< (if x (store-object-id x) large-int)
             (if y (store-object-id y) large-int))
          x
          y))
    (loop for run in (channel-runs channel)
          if (equal (recorder-run-commit run)
                    commit)
            collect run)
    :initial-value nil)))

(defmethod channel-active-run ((channel channel))
  (loop for run in (company-runs (channel-company channel))
        if (and
            (eql channel (recorder-run-channel run))
            (activep run))
          return run))

(defun channel-promoted-runs (channel &key branch)
  (let ((branch (or branch (master-branch channel))))
    (let ((run (active-run channel branch)))
      (loop while run
            collect (unwind-protect
                         run
                      (setf run (recorder-previous-run run)))))))
