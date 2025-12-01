;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(uiop:define-package :screenshotbot/api/recorder-run
  (:use #:cl
        #:alexandria
        #:screenshotbot/api/core
        #:screenshotbot/api/promote
        #:screenshotbot/model/screenshot
        #:screenshotbot/model/image
        #:screenshotbot/model/channel
        #:screenshotbot/promote-api
        #:screenshotbot/api/image
        #:screenshotbot/model/recorder-run)
  (:import-from #:screenshotbot/api/promote
                #:default-promo)
  (:import-from #:screenshotbot/model/company
                #:find-channel
                #:company
                #:find-or-create-channel
                #:find-image-by-id
                #:company-runs)
  (:import-from #:screenshotbot/server
                #:make-thread)
  (:import-from #:screenshotbot/user-api
                #:current-user
                #:current-company)
  (:import-from #:util #:oid)
  (:import-from #:bknr.datastore
                #:store-object-id
                #:with-transaction)
  (:import-from #:screenshotbot/model/company
                #:add-company-run)
  (:import-from #:screenshotbot/dashboard/image
                #:handle-resized-image)
  (:import-from #:util/store
                #:location-for-oid)
  (:import-from #:screenshotbot/model/screenshot-map
                #:make-screenshot-map)
  (:import-from #:screenshotbot/model/recorder-run
                #:shard-key
                #:shard-screenshots
                #:shard-number
                #:shard-count
                #:find-shards
                #:shard
                #:unchanged-run
                #:recorder-run-author
                #:recorder-run-tags
                #:make-recorder-run)
  (:import-from #:util/misc
                #:not-null!
                #:?.)
  (:import-from #:screenshotbot/dashboard/run-page
                #:run-link)
  (:import-from #:core/installation/installation
                #:*installation*
                #:installation-domain)
  (:import-from #:screenshotbot/installation
                #:installation)
  (:import-from #:alexandria
                #:when-let)
  (:import-from #:screenshotbot/model/batch
                #:find-or-create-batch)
  (:import-from #:util/throttler
                #:throttler
                #:throttle!
                #:keyed-throttler)
  (:import-from #:util/events
                #:push-event
                #:with-tracing)
  (:import-from #:screenshotbot/screenshot-api
                #:image-public-url)
  (:import-from #:hunchentoot-extensions
                #:make-full-url)
  (:import-from #:util.cdn
                #:*cdn-domain*)
  (:import-from #:auth/viewer-context
                #:viewer-context-api-key)
  (:import-from #:core/api/model/api-key
                #:transient-api-key
                #:cli-api-key
                #:api-key-permissions)
  (:import-from #:screenshotbot/api/core
                #:api-error)
  (:import-from #:util/hash-lock
                #:hash-lock
                #:with-hash-lock-held)
  (:import-from #:util/threading
                #:with-extras)
  (:export
   #:%recorder-run-post
   #:run-response-id
   #:start-promotion-thread
   #:prepare-recorder-run)
  (:local-nicknames (#:dto #:screenshotbot/api/model)))
(in-package :screenshotbot/api/recorder-run)


(defclass create-run-response (api-response)
  ((type :initform "run")
   (id :type integer
       :initarg :id
       :reader run-response-id)))

(defparameter *synchronous-promotion* nil)

(defvar *non-production-throttler* (make-instance 'keyed-throttler
                                                  :tokens 1200)
  "A throttler for non-production runs (i.e. `ci record` and `ci verify`)")

(defvar *shard-hash-lock* (make-instance 'hash-lock
                                         :test #'equal))


(defun js-boolean (x)
  (string= "true" x))


(flet ((fix (x)
         (let ((x (if (listp x) x (list x))))
           (list (car x)
                 (or (cadr x) (car x))
                 (or (caddr x) 'identity))))
       (nil-if-empty (x) (if (str:emptyp x) nil x)))
 (defparameter *run-meta-fields* (mapcar #'fix `((:pull-request nil ,#'nil-if-empty)
                                                 (:branch-hash :main-branch-hash)
                                                 :build-url
                                                 :merge-base
                                                 :override-commit-hash
                                                 (:commit :commit-hash ,#'nil-if-empty)
                                                 (:main-branch nil ,#'nil-if-empty)
                                                 (:phabricator-diff-id nil ,#'nil-if-empty)
                                                 (:gitlab-merge-request-iid nil ,#'nil-if-empty)
                                                 (:github-repo nil ,#'nil-if-empty)))))


(defapi (nil :uri "/api/run" :method :post) (channel screenshot-records
                                                     commit ;; also in *run-meta-fields*
                                                     (create-github-issue :parameter-type 'js-boolean)
                                                     (is-trunk :parameter-type 'js-boolean)
                                                     (periodic-job-p :parameter-type 'js-boolean)
                                                     (is-clean :parameter-type 'js-boolean))
  (let ((screenshot-records (json-mop:json-to-clos
                             screenshot-records
                             'dto:screenshot-list)))
    (log:info "creating run for ~a" channel)

    (log:info "Got commit as: ~a, clean:~a trunk:~a" commit is-clean is-trunk)

    (multiple-value-bind (resp recorder-run)
        (apply '%recorder-run-post
         :channel channel
         :screenshot-records screenshot-records
         :create-github-issue-p create-github-issue
         :is-trunk is-trunk
         :periodic-job-p periodic-job-p
         :is-clean is-clean
         (loop for field in *run-meta-fields*
               appending
               (let ((field (car field)))
                (list field (hunchentoot:parameter (str:downcase (string field)))))))
      (after-run-created recorder-run)
      resp)))

(defvar *fetch-throttler* (make-instance 'throttler
                                         :tokens 100))

(defapi (api-run-get :uri "/api/run/:oid" :method :get :wrap-success nil) (oid)
  (let ((run (util:find-by-oid oid)))
    ;; If someone has an API key, prevent them from guessing object IDs.
    (throttle! *fetch-throttler* :key (auth:current-company))

    (auth:can-view! run)
    (run-to-dto run :include-screenshots t)))

(defun run-to-dto (run &key include-screenshots)
  (make-instance 'dto:run
                 :id (oid run)
                 :url (quri:render-uri
                       (quri:merge-uris
                        (run-link run)
                        (installation-domain (installation))))
                 :screenshots
                 (when include-screenshots
                  (loop for screenshot in (recorder-run-screenshots run)
                        if (screenshot-image screenshot) ;; only for testing convenience
                          collect
                          (make-instance 'dto:screenshot
                                         :name (screenshot-name screenshot)
                                         :url
                                         (let ((*cdn-domain*
                                                 (or *cdn-domain*
                                                     (installation-domain *installation*))))
                                           (util.cdn:make-cdn
                                            (image-public-url
                                             (screenshot-image screenshot)
                                             :originalp t)))
                                         :image-id (util:oid (screenshot-image screenshot)))))
                 :main-branch-hash (recorder-run-branch run)
                 :commit-hash (recorder-run-commit run)
                 :author (recorder-run-author run)
                 :main-branch (recorder-run-branch run)
                 :merge-base (recorder-run-merge-base run)
                 :channel (?. channel-name (recorder-run-channel run))
                 :tags (recorder-run-tags run)
                 :pull-request (pull-request-url run)))

(defapi (api-run-put :uri "/api/run" :method :put :use-yason t) ()
  (let ((body (hunchentoot:raw-post-data :force-text t)))
    (let ((dto (json-mop:json-to-clos body 'dto:run)))
      (destructuring-bind (resp run channel)
          (%put-run (current-company) dto)
        (declare (ignore resp channel))
        (process-created-run run)))))

(defmethod process-created-run ((run recorder-run))
  (after-run-created run)
  (run-to-dto run))

(defmethod process-created-run ((self shard))
  "We only created a shard, not a run."
  (make-instance 'dto:run
                 :shard-spec (make-instance 'dto:shard-spec
                                            :key (shard-key self)
                                            :number (shard-number self)
                                            :count (shard-count self))))

(defmethod run-or-shard-to-dto (run)
  (run-to-dto run))

(defmethod run-or-shard-to-dto ((self shard))
  (make-instance 'dto:run
                 :shard-spec (make-instance 'dto:shard-spec)))

(defun after-run-created (recorder-run)
  (flet ((promotion ()
           (declare (optimize (debug 3) (speed 0)))
           (log:info "Being promotion logic")
           (start-promotion-thread recorder-run)
           (with-tracing (:warmup-image-caches)
             (with-extras (("run" recorder-run))
              (warmup-image-caches recorder-run)))))
    (cond
      (*synchronous-promotion*
       (promotion))
      (t
       (make-thread
        #'promotion
        :name (format nil "Promotion ~a" (recorder-run-channel recorder-run)))))))

(defmethod warmup-image-caches (run)
  (log:info "Warming up small screenshots for ~s" run)
  (loop for screenshot in (recorder-run-screenshots run)
        do
           (progn
             (handle-resized-image (screenshot-image screenshot)
                                   :small :warmup t)))
  (log:info "Warming up full-page screenshots for ~s" run)
  (loop for screenshot in (recorder-run-screenshots run)
        for i from 0
        do
           (when (> i 100)
             (sleep 1))
           (handle-resized-image (screenshot-image screenshot)
                                 :full-page :warmup t)))

(defmethod warmup-image-caches ((run unchanged-run))
  (values))

(defun %recorder-run-post (&rest args
                           &key channel
                             screenshot-records
                             pull-request
                             github-repo ;; also from meta-field
                             branch ;; also from meta-field
                             create-github-issue-p
                             periodic-job-p
                             (company (current-company))
                             is-trunk is-clean
                           &allow-other-keys)
  (declare (optimize (debug 3) (speed 0))
           (ignore pull-request))
  (apply
   'values
   (let ((dto-run (apply 'make-instance 'dto:run
                         :channel channel
                         :create-github-issue-p create-github-issue-p
                         :screenshots screenshot-records
                         :periodic-job-p periodic-job-p
                         :cleanp is-clean
                         :trunkp is-trunk
                         (loop for field in *run-meta-fields*
                               appending
                               (destructuring-bind (arg field-name fn) field
                                 (list field-name
                                       (funcall fn (getf args arg))))))))
     (%put-run company dto-run))))

(defun emptify (x)
  (if (str:emptyp x) nil x))

(define-condition validation-error (error)
  ((message :initarg :message))
  (:report (lambda (e out)
             (format out (slot-value e 'message)))))

(defmethod validate-dto ((run dto:run))
  (flet ((verify (test message &rest args)
           (unless test
             (error 'validation-error
                    :message
                    (apply #'format nil message args)))))
    (verify (< (length (dto:run-tags run)) 10)
            "Only 10 tags are allowed on a run")
    (dolist (tag (dto:run-tags run))
      (verify (< (length tag) 300)
              "Tag too long: ~a" tag))
    (verify (<= (length (dto:run-metadata run)) 10)
            "Only 10 metadata items are allowed on a run")
    (dolist (metadata (dto:run-metadata run))
      (verify (< (length (dto:metadata-key metadata)) 300)
              "Metadata key too long")
      (verify (< (length (dto:metadata-value metadata)) 10240)
              "Metadata value too long"))
    (when (dto:run-author run)
      (verify (< (length (dto:run-author run)) 100)
              "Author name too long"))

    (when (dto:compare-pixel-tolerance run)
      (verify (<= 0 (dto:compare-pixel-tolerance run) 16)
              "pixel-threshold must be between 0 and 16."))
    (when-let ((shard (dto:shard-spec run)))
      (verify (< (length (dto:shard-spec-key shard)) 200)
              "Shard key too long")
      (verify (<= 0 (dto:shard-spec-number shard)
                  (dto:shard-spec-count shard))
              "Invalid shard number: ~a" (dto:shard-spec-number shard))
      (verify (and
               (integerp (dto:shard-spec-number shard))
               (integerp (dto:shard-spec-count shard)))
              "Shard number and count must be present"))))

(defmethod batch-for-run (company run)
  (when-let ((batch (dto:run-batch run)))
    (find-or-create-batch
     :company company
     :repo (dto:run-repo run)
     :commit (or
              (emptify (dto:override-commit-hash run))
              (dto:run-commit run))
     :name batch
     :pull-request-url (dto:pull-request-url run)
     :phabricator-diff-id (dto:phabricator-diff-id run))))

(define-condition production-run-without-ci-permission (api-error)
  ()
  (:report "Trying to create a CI run with a key that does not have the CI permission."))

(defmethod has-ci-permission-p (api-key)
  (member :ci (api-key-permissions api-key)))

(defmethod has-ci-permission-p ((self cli-api-key))
  nil)

(defmethod has-ci-permission-p ((self transient-api-key))
  t)

(defmethod %put-run (company (run dto:run) &key (api-key
                                                 (viewer-context-api-key
                                                  (auth:viewer-context
                                                   hunchentoot:*request*))))
  (unless (dto:trunkp run)
    (throttle! *non-production-throttler* :key (not-null! (current-user))))

  (validate-dto run)

  (when (and
         (gk:check :api-key-roles company)
         (dto:trunkp run)
         api-key)
    (unless (has-ci-permission-p api-key)
      (error 'production-run-without-ci-permission)))

  (let* ((channel (find-or-create-channel company (dto:run-channel run)))
         (screenshots (with-tracing ("screenshot-records-api-to-internal")
                       (screenshot-records-api-to-internal
                        company
                        channel
                        (dto:run-screenshots run)))))
    (cond
      ((dto:shard-spec run)
       (let ((shard (dto:shard-spec run)))
         (with-hash-lock-held ((list company (dto:shard-spec-key shard))
                               *shard-hash-lock*)
           #+nil
           (push-event :create.shard :name (dto:shard-spec-key shard) :company company)
           (let ((persisted-shard (make-instance 'shard
                                                 :company company
                                                 :channel channel
                                                 :key (dto:shard-spec-key shard)
                                                 :number (dto:shard-spec-number shard)
                                                 :count (dto:shard-spec-count shard)
                                                 :screenshots screenshots)))
             (cond
              ((shard-complete-p channel (dto:shard-spec-key shard))
               (prog1
                   (%put-run-helper run
                                    :company company
                                    :channel channel
                                    :screenshots (build-shard-screenshots
                                                  channel
                                                  (dto:shard-spec-key shard)))
                 (mapcar #'bknr.datastore:delete-object (find-shards channel
                                                                     (dto:shard-spec-key shard)))))
              (t
               (list nil persisted-shard nil)))))))
      (t
       (%put-run-helper run :company company
                            :channel channel
                            :screenshots screenshots)))))

(defun build-shard-screenshots (channel shard-key)
  (check-type channel channel)
  (when-let* ((shards (find-shards channel shard-key))
              (count (shard-count (car shards))))
    (dolist (shard shards)
      (assert (= count (shard-count shard))))
    (let ((res (make-array count :initial-element nil)))
      (dolist (shard shards)
        (util:or-setf
         (aref res (mod (shard-number shard) count))
         shard))
      (cond
        ((not (every #'identity (loop for shard across res
                                      collect shard)))
         (values nil nil))
        (t
         (values
          (loop for shard across res
                appending (shard-screenshots shard))
          t))))))

(defun shard-complete-p (channel shard-key)
  (nth-value 1 (build-shard-screenshots channel shard-key)))

(defun parse-metadata (metadata-list)
  (loop for metadata in metadata-list
        collect (cons (dto:metadata-key metadata)
                      (dto:metadata-value metadata))))

(defun %put-run-helper (run &key
                              (company (error "provide :company"))
                              (channel (error "provide :channel"))
                              (screenshots (error "provide :screenshots")))
  "Step two of %put-run, after validations and after we've processed
somethings like SCREENSHOTS. In particular SCREENSHOTS might be
computed differently if we're using sharding."
  (let* ((batch (batch-for-run company run))
         (recorder-run (make-recorder-run
                        :company company
                        :channel channel
                        :batch batch
                        :screenshots screenshots
                        :metadata (acons
                                   "client-version" (when (boundp 'hunchentoot:*request*)
                                                      (hunchentoot:header-in* :x-client-version))
                                   (parse-metadata (dto:run-metadata run)))
                        :commit-hash (dto:run-commit run)
                        :author (dto:run-author run)
                        :create-github-issue-p (dto:should-create-github-issue-p run)
                        :trunkp (dto:trunkp run)
                        :periodic-job-p (dto:periodic-job-p run)
                        :cleanp (dto:cleanp run)
                        :pull-request (dto:pull-request-url run)
                        :release-branch-p (dto:release-branch-p run)
                        :branch (cond
                                  ((dto:release-branch-p run)
                                   ;; Heads up: we're logging the
                                   ;; old main-branch in the
                                   ;; :run-created event below.
                                   (dto:work-branch run))
                                  (t
                                   (dto:main-branch run)))
                        :work-branch (dto:work-branch run)
                        :branch-hash (dto:main-branch-hash run)
                        :override-commit-hash (dto:override-commit-hash run)
                        :build-url (dto:build-url run)
                        :merge-base (dto:merge-base run)
                        :phabricator-diff-id (dto:phabricator-diff-id run)
                        :gitlab-merge-request-iid (dto:gitlab-merge-request-iid run)
                        :github-repo (dto:run-repo run)
                        :tags (dto:run-tags run)
                        :compare-threshold (dto:compare-threshold run)
                        :compare-tolerance (dto:compare-pixel-tolerance run))))

    (with-transaction ()
      (setf (channel-branch channel) (dto:main-branch run)))
    (with-transaction ()
      (setf (github-repo channel)
            (dto:run-repo run)))
    (push-event :run-created
                :oid (oid recorder-run)
                :company (oid company)
                ;; We're logging this in case we want to verify the
                ;; screenshot-map is behaving correctly.
                :original-main-branch (dto:work-branch run)
                :num-screenshots (length screenshots)
                ;; This let's us monitor that a specific company is
                ;; using the right domain name. See T1124.
                :api-hostname (when (boundp 'hunchentoot:*request*)
                                (or
                                 (hunchentoot:header-in* :x-forwarded-host)
                                 (hunchentoot:host))))
    (prepare-recorder-run :run recorder-run)
    (list
     (make-instance 'create-run-response
                    :id (store-object-id recorder-run))
     recorder-run
     channel)))

(defun prepare-recorder-run (&key (run (error "must provide run")))
  "Common preparation steps for a recorder-run, even before the
promotion thread starts. Used by the API and by Replay"
  (let ((channel (recorder-run-channel run))
        (company (recorder-run-company run)))
    (check-type channel channel)
    (check-type company company)

    (add-company-run company run)
    (bt:with-lock-held ((channel-lock channel))
      (bt:condition-notify (channel-cv channel)))))

(defun start-promotion-thread (run)
  (with-tracing (:promotion-time :run (format nil "~a" run))
    (let ((channel (recorder-run-channel run)))
      (unwind-protect
           (with-promotion-log (run)
             (unwind-protect
                  (let ((promoter (make-instance 'default-promoter)))
                    (maybe-promote promoter run)
                    (maybe-send-tasks promoter run))
               (with-transaction ()
                 (setf (promotion-complete-p run) t))
               (bt:with-lock-held ((channel-lock channel))
                 (bt:condition-notify (channel-cv channel)))))))))

(defun screenshot-records-api-to-internal (company channel screenshot-records)
  "Convert the json list of screenshot-records to a list of SCREENSHOT
  objects"
  (loop for rec in screenshot-records
        collect
        (let ((name (dto:screenshot-name rec))
              (image-id (dto:screenshot-image-id rec))
              (lang (dto:screenshot-lang rec))
              (device (dto:screenshot-device rec)))
          (assert name)
          (assert image-id)
          (let ((image (find-image-by-id company image-id)))
            (unless image
              (error "could not find image-id: ~a" image-id))
            (make-screenshot-for-channel
             channel
             :name name
             :lang lang
             :device device
             :image image)))))

(defun make-screenshot-for-channel (channel &rest args)
  (let ((masks (assoc-value (masks channel)
                            (getf args :name)
                            :test 'string=)))
   (apply 'make-screenshot :masks masks args)))

(defapi (%find-base-run :uri "/api/find-base-run" :wrap-success nil) (channel commit)
  "Find an appropriate base run for the given channel and commit.

If we could not find an appropriate run, then we'll return nil"
  (let* ((channel-name channel)
         (channel (find-channel (auth:current-company) channel-name)))
    (unless channel
      (api-error "No such channel: ~a" channel-name))
    (let ((run (production-run-for channel :commit commit)))
      (cond
        ((null run)
         nil)
        (t
         (auth:can-view! run)
         (run-to-dto run))))))
