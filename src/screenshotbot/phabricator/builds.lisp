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

(with-class-validation
 (defclass build-info (store-object)
   ((diff :initarg :diff
          :reader build-info-diff)
    (revision :initarg :revision
              :reader build-info-revision)
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
    (ts :initarg :ts
        :reader ts))
   (:metaclass persistent-class)
   (:default-initargs :ts (get-universal-time))))

(defmethod initialize-transient-instance :after ((self build-info))
  (setf (gethash (cons (company self)
                       (build-info-diff self))
                 *build-info-index*)
        self))

(defun find-build-info (company diff)
  (gethash (cons company diff) *build-info-index*))

(def-easy-macro in-future (&fn fn)
  (with-screenshotbot-kernel ()
    (future
      (funcall fn))))

(defapi (%update-build :uri "/phabricator/update-build" :method :post)
        ((diff :parameter-type 'integer) (revision :parameter-type 'integer) target-phid
         build-phid)
  (let* ((company (current-company))
         (build-info (find-build-info company diff)))
    (cond
      (build-info
       (with-transaction ()
         (setf (target-phid build-info) target-phid
               (build-phid build-info) build-phid)))
      (t
       (setf build-info
             (make-instance 'build-info
                            :diff diff
                            :revision revision
                            :target-phid target-phid
                            :build-phid build-phid
                            :company company))))

    (in-future ()
      (a:when-let ((phab-instance (phab-instance-for-company company))
                   (type (build-info-status build-info)))
        (%actually-update-status
         phab-instance
         build-info
         type :details (build-info-details build-info)))))

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

(defmethod %actually-update-status ((phab phab-instance) (self build-info) type
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

(defmethod update-status ((phab phab-instance) (self build-info) type
                          &key details)
  (let ((prev-status (build-info-status self)))
    (with-transaction ()
      (setf (build-info-status self) type
            (build-info-details self) details))
    (cond
      (prev-status
       ;; when we get the update-build callback, we'll update the build
       (%send-message phab (build-phid self) :restart))
      (t
       (%actually-update-status phab self type :details details)))))

#|
(update-status *phab*
(car (reverse (bknr.datastore:class-instances 'build-info)))

:pass
:details "https://www.google.com"
)
|#
