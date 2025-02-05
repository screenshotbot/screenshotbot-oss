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
                #:was-promoted-p
                #:unchanged-run-other-commit
                #:unchanged-run-for-commit
                #:unchanged-runs-for-commit
                #:remove-run-from-channel
                #:recorder-run
                #:recorder-run-commit
                #:master-branch
                #:active-run
                #:recorder-previous-run
                #:channel-runs
                #:push-run-to-channel
                #:github-repo
                #:activep
                #:publicp
                #:recorder-run-channel)
  (:import-from #:screenshotbot/model/company
                #:company
                #:company-runs)
  (:import-from #:util/store
                #:with-class-validation)
  (:import-from #:screenshotbot/model/review-policy
                #:disallow-author-review-policy
                #:anyone-can-review)
  (:import-from #:util/store/store-migrations
                #:def-store-migration)
  (:import-from #:auth/viewer-context
                #:viewer-context-api-key
                #:api-viewer-context)
  (:import-from #:screenshotbot/model/api-key
                #:api-key-company)
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
   #:all-active-runs
   #:channel-subscribers
   #:commit-to-run-map
   #:channel-slack-channels
   #:review-policy-name
   #:channel-deleted-p)

  ;; forward decls
  (:export
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
     :accessor company
     :documentation "This slot might be NIL if the channel was deleted, which can indirectly lead to background promotion threads crashing, but in general will be safe.")
    (active-runs
     :initform nil
     :accessor all-active-runs
     :relaxed-object-reference t
     :documentation "Mapping from branch name to active run as an alist")
    (branch :initarg :branch
            :accessor channel-branch)
    (github-repo :initarg :github-repo
                 :initform nil
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
     :documentation "Deprecated: do not use. See runs-for-channel instead")
    (%commit-to-run-map
     :initform (fset:empty-map)
     :transient t
     :accessor commit-to-run-map)
    (lock
     :initform (bt:make-lock)
     :transient t
     :reader channel-lock)
    (promotion-lock
     :initform (bt:make-lock)
     :transient t
     :reader channel-promotion-lock)
    (%subscribers
     :initform nil
     :accessor channel-subscribers)
    (cv
     :initform (bt:make-condition-variable :name "channel-cv")
     :transient t
     :reader channel-cv)
    (created-at
     :initform (local-time:timestamp-to-universal (local-time:now))
     :accessor %created-at)
    (%slack-channels
     :initform nil
     :accessor channel-slack-channels)
    (%review-policy
     :initarg :review-policy
     :accessor review-policy-name)
    (%allow-public-badge-p
     :initarg :allow-public-badge-p
     :accessor allow-public-badge-p))
   (:metaclass persistent-class)
   (:default-initargs :review-policy :allow-author
                      :allow-public-badge-p nil)))

(defmethod channels-for-company (company)
  (loop for channel in (bknr.datastore:class-instances 'channel)
        if (eql company (company channel))
          collect channel))

(defmethod print-object ((self channel) stream)
  (format stream "#<CHANNEL ~a>" (ignore-errors (channel-name self))))

(defmethod review-policy-name :around ((self channel))
  (or
   (ignore-errors (call-next-method))
   :allow-author))

(defmethod review-policy ((self channel))
  (ecase (review-policy-name self)
    (:allow-author
     (make-instance 'anyone-can-review))
    (:disallow-author
     (make-instance 'disallow-author-review-policy))))

(defmethod channel-company ((channel channel))
  (company channel))

(defun all-channels ()
  (store-objects-with-class 'channel))

(defmethod github-repo ((channel channel))
  (or
   (assoc-value *channel-repo-overrides* channel)
   (%%github-repo channel)))

#-lispworks
(defvar *updatef-lock* (bt:make-lock))

(defmacro updatef (map key fn)
  (let ()
    `(let ((k ,key)
           (ff ,fn))

       (flet ((update (m)
                (fset:with m
                           k
                           (funcall ff (fset:lookup m k)))))
         #+lispworks
         (atomics:atomic-update
          ,map #'update)
         #-lispworks
         (bt:with-lock-held (*updatef-lock*)
           (setf
            ,map (update ,map)))))))

(defmethod push-run-to-channel ((channel channel) run)
  (when-let ((commit (recorder-run-commit run)))
    (updatef (slot-value channel '%commit-to-run-map)
             commit (lambda (items)
                      (list* run items)))))

(defmethod remove-run-from-channel ((channel channel) run)
  (when-let ((commit (recorder-run-commit run)))
    (updatef (slot-value channel '%commit-to-run-map)
             commit
             (lambda (items)
               (remove run items)))))

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
  (setf (was-promoted-p run) t)
  (%set-active-run channel branch run))

(defmethod (setf active-run) ((run recorder-run) (channel channel) (branch null))
  (values))

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

(defmethod created-at (x)
  (local-time:universal-to-timestamp (%created-at x)))

(defmethod can-view ((channel channel) user)
  (auth:can-view-with-normal-viewer-context
   user channel))

(defmethod auth:can-viewer-view (vc (channel channel))
  (or
   (publicp channel)
   (auth:can-viewer-view vc (company channel))))

(defmethod auth:can-viewer-view ((vc api-viewer-context)
                                 (channel channel))
  (or
   (publicp channel)
   (eql (api-key-company (viewer-context-api-key vc))
        (company channel))))

(defmethod production-run-for ((channel channel)
                               &key commit
                                 (seen (fset:empty-set)))
  ;; currently returns the oldest run for a given commit
  (let ((unchanged-run (unchanged-run-for-commit
                        channel commit)))
    (cond
      ((fset:contains? seen commit)
       nil)
      (unchanged-run
       (production-run-for channel
                           :commit (unchanged-run-other-commit unchanged-run)
                           :seen (fset:with seen commit)))
      (t
       (let ((large-int most-positive-fixnum))
         (reduce
          (lambda (x y)
            (if (< (if x (store-object-id x) large-int)
                   (if y (store-object-id y) large-int))
                x
                y))
          (fset:lookup
           (commit-to-run-map channel)
           commit)
          :initial-value nil))))))

(defmethod channel-active-run ((channel channel))
  (loop for run in (company-runs (channel-company channel))
        if (and
            (eql channel (recorder-run-channel run))
            (activep run))
          return run))

(defun channel-promoted-runs (channel &key branch
                                        (iterator nil))
  "Get the list of promoted-runs. If iterator is t, it returns a function
 that always returns two values: the next promoted-run, and whether
 this is an eof, or end of list. It is safe to call the iterator
 multiple times at the end."
  (let ((branch (or branch (master-branch channel))))
    (let ((run (active-run channel branch)))
      (flet ((iterator ()
               (when run
                (let ((next run)
                      (previous-run (recorder-previous-run run)))
                  (setf run previous-run)
                  (values next (not (null next)))))))

        (cond
          (iterator
           #'iterator)
          (t
           (loop for run = (iterator)
                 for i from 0 to 1000
                 while run
                 collect run)))))))

(def-store-migration ("Ensure allow-public-badge-p is bound" :version 20)
  (ensure-slot-boundp 'channel '%allow-public-badge-p))

(defmethod channel-deleted-p ((channel channel))
  (not (company channel)))

(defmethod repos-for-company (company)
  (let ((channels (channels-for-company company)))
    (fset:convert
     'list
     (fset:convert
      'fset:set
      (mapcar #'github-repo channels)))))
