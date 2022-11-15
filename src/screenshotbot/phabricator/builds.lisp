;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/phabricator/builds
  (:use #:cl)
  (:import-from #:screenshotbot/server
                #:defhandler)
  (:import-from #:screenshotbot/api/core
                #:defapi)
  (:import-from #:bknr.indices
                #:hash-index)
  (:import-from #:bknr.datastore
                #:store-object)
  (:import-from #:bknr.datastore
                #:persistent-class)
  (:import-from #:screenshotbot/user-api
                #:current-company)
  (:import-from #:util/phabricator/conduit
                #:phab-instance
                #:call-conduit)
  (:import-from #:util/store
                #:with-class-validation)
  (:import-from #:screenshotbot/model/company
                #:company)
  (:import-from #:bknr.indices
                #:unique-index)
  (:import-from #:bknr.datastore
                #:initialize-transient-instance)
  (:import-from #:bknr.datastore
                #:with-transaction)
  (:import-from #:screenshotbot/async
                #:with-screenshotbot-kernel)
  (:import-from #:lparallel.promise
                #:future)
  (:import-from #:screenshotbot/phabricator/plugin
                #:phab-instance-for-company)
  (:import-from #:easy-macros
                #:def-easy-macro)
  (:import-from #:screenshotbot/model/core
                #:ensure-slot-boundp)
  (:local-nicknames (#:a #:alexandria)))
(in-package :screenshotbot/phabricator/builds)

(defvar *build-info-index* (make-hash-table :test #'equal))

(defvar *lock* (bt:make-lock))


(with-class-validation
 (defclass build-info (store-object)
   ((diff :initarg :diff
          :reader build-info-diff)
    (revision :initarg :revision
              :accessor build-info-revision)
    (target-phid :initarg :target-phid
                 :accessor target-phid)
    (build-phid :initarg :build-phid
                :accessor build-phid)
    (company :initarg :company
             :reader company)
    (status :initarg :status
            :initform nil
            :accessor build-info-status)
    (details :initarg :details
             :initform nil
             :accessor build-info-details)
    (needs-sync-p :initform nil
                  :accessor needs-sync-p
                  :documentation "The status has been updated, but we've not synced to Phabricator yet,
 because we haven't gotten a callback, or because we've sent out a
 restart command and awaiting its response ")
    (ts :initarg :ts
        :reader ts))
   (:metaclass persistent-class)
   (:default-initargs :ts (get-universal-time))))

(defmethod initialize-transient-instance :after ((self build-info))
  (when (and
         (slot-boundp self 'company)
         (slot-boundp self 'diff))
   (setf (gethash (cons (company self)
                        (build-info-diff self))
                  *build-info-index*)
         self)))

(defun find-build-info (company diff)
  (gethash (cons company diff) *build-info-index*))

(def-easy-macro in-future (&fn fn)
  (with-screenshotbot-kernel ()
    (future
      (funcall fn))))

(defmacro if-setf (place expr)
  `(let ((val ,expr))
     (when val
       (setf ,place val))))

(defmethod make-or-update-build-info (company (diff number)
                                      &key )
  (let ((build-info (find-build-info company diff)))
    (cond
      (build-info
       (with-transaction ())
       build-info)
      (t
       (make-instance 'build-info
                      :diff diff
                      :company company)))))

(defapi (%update-build :uri "/phabricator/update-build" :method :post)
        ((diff :parameter-type 'integer) (revision :parameter-type 'integer) target-phid
         build-phid)
  (assert diff)
  (assert revision)
  (assert target-phid)
  (assert build-phid)
  (let* ((company (current-company)))
    (assert company)
    (in-future ()
      (bt:with-lock-held (*lock*)
        (let ((build-info (make-or-update-build-info
                           company diff)))
          (with-transaction ()
            (setf (build-info-revision build-info) revision)
            (setf (target-phid build-info) target-phid)
            (setf (build-phid build-info) build-phid))
          (when (needs-sync-p build-info)
            (let ((phab-instance (phab-instance-for-company company))
                         (type (build-info-status build-info)))
              (%actually-update-status
               phab-instance
               build-info
               type :details (build-info-details build-info)))
            (with-transaction ()
              (setf (needs-sync-p build-info) nil)))))))
  "OK")

(defmethod %send-message ((phab phab-instance)
                          phid
                          type &key unit)
  ;;(check-type unit (or null string))
  (let ((type (str:downcase type)))
    (let ((args `(,@ (when unit
                       `(("unit" . ,unit)))
                     ("receiver" . ,phid)
                     ("type" . ,type))))
      (call-conduit
       phab
       "harbormaster.sendmessage"
       args))))

(defun %actually-update-status (phab  self type
                                &key details)
  "Immediately send the Harbormaster message"
  (log:info "Updating: D~a" (build-info-revision self))
  (%send-message phab
                 (target-phid self)
                 type
                 :unit
                 (list
                  (a:alist-hash-table
                   `(("name" . "Screenshot Tests")
                     ("result" . ,(str:downcase type))
                     ("details" . ,(or details "dummy tdetails"))
                     ("format" . "remarkup"))))))

(defun got-initial-callback-p (build-info)
  (slot-boundp build-info 'build-phid))

(defmethod update-diff-status (company (diff number)
                               status &key details)
  (bt:with-lock-held (*lock*)
    (let ((build-info
            (make-or-update-build-info
             company diff))
          (phab (phab-instance-for-company company)))
      (let ((original-status (build-info-status build-info)))
        (with-transaction ()
          (setf (build-info-status build-info) status
                (build-info-details build-info) details))
        (flet ((update-needs-sync ()
                 (with-transaction ()
                   (setf (needs-sync-p build-info) t))))
          (cond
            ((needs-sync-p build-info)
             ;; We don't need to do anything right now, we're still
             ;; waiting a callback
             (values))
            ((and (not original-status)
                  (got-initial-callback-p build-info))
             ;; First time we're sending a message, but we already
             ;; have a callback, so we can do this immediately.
             (%actually-update-status phab build-info
                                      status :details details)
             )
            ((got-initial-callback-p build-info)
             ;; So needs-sync is nil, and we got an initial callback, so
             ;; we can immediately send the message.
             (send-restart phab build-info)
             (update-needs-sync))
            (t
             ;; We haven't got even our first callback at this point,
             ;; so we just update the needs-sync.
             (update-needs-sync))))))))

(defmethod send-restart ((phab phab-instance) (self build-info))
  (%send-message phab (build-phid self) :restart))

#|
(update-status *phab*
(car (reverse (bknr.datastore:class-instances 'build-info)))

:pass
:details "https://www.google.com"
)
|#
