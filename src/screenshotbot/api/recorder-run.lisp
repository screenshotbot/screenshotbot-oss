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
                #:company
                #:find-or-create-channel
                #:find-image-by-id
                #:company-runs)
  (:import-from #:screenshotbot/server
                #:make-thread)
  (:import-from #:screenshotbot/user-api
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

(defun run-to-dto (run)
  (make-instance 'dto:run
                 :id (oid run)))

(defapi (api-run-put :uri "/api/run" :method :put :use-yason t) ()
  (let ((body (hunchentoot:raw-post-data :force-text t)))
    (let ((dto (json-mop:json-to-clos body 'dto:run)))
      (destructuring-bind (resp run channel)
          (%put-run (current-company) dto)
        (declare (ignore resp channel))
        (after-run-created run)
        (run-to-dto run)))))

(defun after-run-created (recorder-run)
  (flet ((promotion ()
           (declare (optimize (debug 3) (speed 0)))
           (log:info "Being promotion logic")
           (start-promotion-thread recorder-run)
           (time
            (warmup-image-caches recorder-run))))
    (cond
      (*synchronous-promotion*
       (promotion))
      (t
       (make-thread
        #'promotion
        :name (format nil "Promotion ~a" (recorder-run-channel recorder-run)))))))

(defun warmup-image-caches (run)
  (log:info "Warming up small screenshots for ~s" run)
  (loop for screenshot in (recorder-run-screenshots run)
        do
           (progn
             (handle-resized-image (screenshot-image screenshot)
                                   :small :warmup t)))
  (log:info "Warming up full-page screenshots for ~s" run)
  (loop for screenshot in (recorder-run-screenshots run)
        do
           (progn
             (handle-resized-image (screenshot-image screenshot)
                                   :full-page :warmup t))))

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

(defmethod backup-location ((recorder-run recorder-run))
  (location-for-oid #P"cl-store/runs/"
                    (oid recorder-run :stringp nil)))

(defmethod write-dto-backup ((recorder-run recorder-run)
                             (run dto:run))
  "Writes a backup of the DTO to disk. We do clever things with the data in persistent storage,
especially with respect to the list of screenshots, so this will make
it easier for us to recover, or even validate the model."
  (let ((filename (backup-location recorder-run)))
    (with-open-file (out filename :direction :output
                                  :if-exists :supersede)
      (json-mop:encode run out))))

(defmethod %put-run (company (run dto:run))
  (let* ((channel (find-or-create-channel company (dto:run-channel run)))
         (screenshots (screenshot-records-api-to-internal
                       company
                       channel
                       (dto:run-screenshots run)))
         (recorder-run (make-instance
                        'recorder-run
                        :company company
                        :channel channel
                        :screenshots screenshots
                        :commit-hash (dto:run-commit run)
                        :create-github-issue-p (dto:should-create-github-issue-p run)
                        :trunkp (dto:trunkp run)
                        :periodic-job-p (dto:periodic-job-p run)
                        :cleanp (dto:cleanp run)
                        :pull-request (dto:pull-request-url run)
                        :branch (dto:main-branch run)
                        :branch-hash (dto:main-branch-hash run)
                        :override-commit-hash (dto:override-commit-hash run)
                        :build-url (dto:build-url run)
                        :merge-base (dto:merge-base run)
                        :phabricator-diff-id (dto:phabricator-diff-id run)
                        :gitlab-merge-request-iid (dto:gitlab-merge-request-iid run)
                        :github-repo (dto:run-repo run)
                        :compare-threshold (dto:compare-threshold run))))

    (write-dto-backup recorder-run run)

    (with-transaction ()
      (setf (channel-branch channel) (dto:main-branch run)))
    (with-transaction ()
      (setf (github-repo channel)
            (dto:run-repo run)))
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
               (bt:condition-notify (channel-cv channel))))))))

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

(defun recorder-run-verify (recorder-run)
  ;; try not to hold onto the DB for long periods of time
  (dolist (im (mapcar 'screenshot-image (recorder-run-screenshots recorder-run)))
    (verify-image im)))
