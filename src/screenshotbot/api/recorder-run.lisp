;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(pkg:define-package :screenshotbot/api/recorder-run
    (:use #:cl
          #:alexandria
          #:./core
          #:./promote
          #:../model/screenshot
          #:../model/image
          #:../model/channel
          #:../promote-api
          #:./image
          #:../model/recorder-run)
  (:import-from #:./promote
                #:default-promo)
  (:import-from #:../model/company
                #:find-or-create-channel
                #:find-image-by-id
                #:company-runs)
  (:import-from #:../server
                #:make-thread)
  (:import-from #:../user-api
                #:current-company)
  (:import-from #:util
                #:oid)
  (:import-from #:bknr.datastore
                #:store-object-id
                #:with-transaction)
  (:export #:%recorder-run-post
           #:run-response-id
           #:start-promotion-thread))


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
                                                 :branch-hash
                                                 :build-url
                                                 :merge-base
                                                 (:commit :commit-hash ,#'nil-if-empty)
                                                 (:branch nil ,#'nil-if-empty)
                                                 (:phabricator-diff-id nil ,#'nil-if-empty)
                                                 (:gitlab-merge-request-iid nil ,#'nil-if-empty)
                                                 (:github-repo nil ,#'nil-if-empty)))))


(defapi (nil :uri "/api/run" :method :post) (channel screenshot-records
                                                     commit ;; also in *run-meta-fields*
                                                     (create-github-issue :parameter-type 'js-boolean)
                                                     (is-trunk :parameter-type 'js-boolean)
                                                     (is-clean :parameter-type 'js-boolean))
  (let ((screenshot-records (json:decode-json-from-string screenshot-records)))
    (log:info "creating run for ~a" channel)

    (log:info "Got commit as: ~a, clean:~a trunk:~a" commit is-clean is-trunk)

    (multiple-value-bind (resp recorder-run channel-obj)
        (apply '%recorder-run-post
         :channel channel
         :screenshot-records screenshot-records
         :create-github-issue-p create-github-issue
         :is-trunk is-trunk
         :is-clean is-clean
         (loop for field in *run-meta-fields*
               appending
               (let ((field (car field)))
                (list field (hunchentoot:parameter (str:downcase (string field)))))))
      (flet ((promotion ()
               (declare (optimize (debug 3) (speed 0)))
               (log:info "Being promotion logic")
               (start-promotion-thread channel-obj recorder-run)))
        (cond
          (*synchronous-promotion*
           (promotion))
          (t
           (make-thread #'promotion
                        :name (format nil "Promotion ~a" channel)))))
      resp)))

(defun %recorder-run-post (&rest args
                           &key channel
                             screenshot-records
                             pull-request
                             github-repo ;; also from meta-field
                             branch ;; also from meta-field
                             create-github-issue-p
                             (company (current-company))
                             is-trunk is-clean
                           &allow-other-keys)
  (declare (optimize (debug 3) (speed 0)))
  (apply
   'values
   (flet ((nil-if-empty (x) (if (str:emptyp x) nil x)))
    (let* ((channel (find-or-create-channel company channel))
           (run (apply 'make-instance 'recorder-run
                               :channel channel
                               :company company
                               :create-github-issue-p create-github-issue-p
                               :screenshots (screenshot-records-api-to-internal
                                             company
                                             channel
                                             screenshot-records)
                               :cleanp is-clean
                               :trunkp is-trunk
                               (loop for field in *run-meta-fields*
                                     appending
                                     (destructuring-bind (arg field-name fn) field
                                      (list field-name
                                            (funcall fn (getf args arg))))))))

      (with-transaction ()
        (push run (company-runs company))
        (setf (channel-branch channel) branch))

      (with-transaction ()
        (setf (github-repo channel)
              (nil-if-empty github-repo)))

      (log:trace "records: ~s" screenshot-records)
      ;; now add all the screenshots

      (bt:with-lock-held ((channel-lock channel))
        (bt:condition-notify (channel-cv channel)))

      (list
       (make-instance 'create-run-response
                      :id (store-object-id run))
       run
       channel ;; possibly unsaved channel, gets saved if the run is promoted
       )))))

(let ((sem #+lispworks (mp:make-semaphore :name "promoter semaphore" :count 10)))
  (defun start-promotion-thread (channel run)
    #+lispworks
    (mp:semaphore-acquire sem)
    (let ((channel (or channel (recorder-run-channel run))))
     (unwind-protect
          (with-promotion-log (run)
            (unwind-protect
                 (let ((promoter (make-instance 'default-promoter)))
                   (maybe-promote promoter run)
                   (maybe-send-tasks promoter run))
              (with-transaction ()
                (setf (promotion-complete-p run) t))
              (bt:with-lock-held ((channel-lock channel))
                (bt:condition-notify (channel-cv channel)))))
       #+lispworks
       (mp:semaphore-release sem)))))

(defun screenshot-records-api-to-internal (company channel screenshot-records)
  "Convert the json list of screenshot-records to a list of SCREENSHOT
  objects"
  (loop for rec in screenshot-records
        collect
        (let ((name (assoc-value rec :name))
              (image-id (assoc-value rec :image-id))
              (lang (assoc-value rec :lang))
              (device (assoc-value rec :device)))
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
