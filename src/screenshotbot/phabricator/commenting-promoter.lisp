;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(uiop:define-package :screenshotbot/phabricator/commenting-promoter
  (:use #:cl
        #:alexandria
        #:screenshotbot/promote-api
        #:screenshotbot/model/recorder-run
        #:screenshotbot/model/report
        #:screenshotbot/model/channel
        #:screenshotbot/compare)
  (:import-from #:screenshotbot/server
                #:*domain*)
  (:import-from #:screenshotbot/diff-report
                #:diff-report-title)
  (:import-from #:screenshotbot/installation
                #:installation-domain
                #:installation)
  (:import-from #:screenshotbot/promoter/async-promoter
                #:on-promote
                #:async-promoter)
  (:export
   #:commenting-promoter
   #:add-comment))
(in-package :screenshotbot/phabricator/commenting-promoter)

(defclass commenting-promoter (promoter)
  ((comment :initform nil
     :accessor promoter-comment)
   (report :initform nil
           :accessor promoter-report))
  (:documentation "A promoter that just comments when done. The actual
  comment might vary.."))

(defmethod add-comment ((promoter commenting-promoter) comment)
  (error "add-comment not implemented for ~S" promoter))

(defmethod maybe-promote ((promoter commenting-promoter) run)
  (restart-case
      (let* ((channel (recorder-run-channel run))
             (merge-base (recorder-run-merge-base run))
             (base-run (production-run- for channel :commit merge-base)))
        (flet ((comment (fmt &rest args)
                 (setf (promoter-comment promoter) (apply 'format nil fmt args))))
         (cond
           ((not base-run)
            (comment "Parent commit ~A not available on Screenshotbot"
              merge-base))
           (t
            (let ((diff-report (make-diff-report run base-run)))
              (cond
                ((diff-report-empty-p diff-report)
                 ;; todo: comment that no changes happened?
                 nil)
                (t
                 (let ((report (make-instance 'report
                                         :run run
                                         :previous-run base-run
                                         :channel (when run (recorder-run-channel run))
                                         :title (diff-report-title diff-report))))
                   ;; todo: set up acceptable
                   #+nil
                   (with-transaction ()
                     (setf (report-acceptable report)
                           (make-instance 'gitlab-acceptable
                                           :report report)))
                   (setf (promoter-report promoter)
                         report)
                   (comment
                     (format
                      nil
                      "Screenshot changes: ~a ~a/report/~a"
                      (diff-report-title diff-report)
                      (installation-domain (installation))
                      (util:oid report)))))))))))
    (restart-maybe-promote ()
      (maybe-promote promoter run))))

(defmethod maybe-send-tasks ((promoter commenting-promoter) run)
  (restart-case
      (let ((comment (promoter-comment promoter)))
        (when comment
          (add-comment promoter comment)))
    (restart-maybe-send-tasks-dangerously ()
      (maybe-send-tasks promoter run))))
