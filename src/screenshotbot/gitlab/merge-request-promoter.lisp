;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(uiop:define-package :screenshotbot/gitlab/merge-request-promoter
  (:use #:cl
        #:alexandria
        #:screenshotbot/promote-api
        #:util/java
        #:screenshotbot/model/channel
        #:screenshotbot/compare
        #:screenshotbot/model/report
        #:screenshotbot/model/recorder-run
        #:screenshotbot/gitlab/repo
        #:bknr.datastore)
  (:nicknames
   :screenshotbot/pro/gitlab/merge-request-promoter) ;; for bknr
  (:import-from #:screenshotbot/model/report
                #:base-acceptable)
  (:import-from #:screenshotbot/gitlab/repo
                #:*gitlab-url*
                #:repo-access-token)
  (:import-from #:screenshotbot/diff-report
                #:diff-report-title)
  (:import-from #:screenshotbot/installation
                #:installation
                #:installation-domain)
  (:import-from #:screenshotbot/user-api
                #:current-company)
  (:import-from #:screenshotbot/gitlab/settings
                #:company
                #:gitlab-token
                #:gitlab-url
                #:gitlab-settings-for-company)
  (:export
   #:merge-request-promoter
   #:gitlab-acceptable))
(in-package :screenshotbot/gitlab/merge-request-promoter)


(named-readtables:in-readtable java-syntax)

(defclass merge-request-promoter (promoter)
  ((comments :initform nil
             :accessor comments)
   (report :initform nil
           :accessor promoter-report)))

(defun safe-get-mr-id (run)
  (let ((mr-id (gitlab-merge-request-iid run)))
    (unless (str:emptyp mr-id)
      (parse-integer mr-id))))

(defun gitlab-request (repo url &key (method :get) content)
  (let ((settings (gitlab-settings-for-company (company repo))))
   (dex:request (format nil "~a/api/v4~a" (gitlab-url settings) url)
                :method method
                :headers `(("PRIVATE-TOKEN" . ,(gitlab-token settings)))
                :content content)))

(defclass merge-request ()
  ((base-sha :initarg :base-sha
             :accessor base-sha)))

(defun get-merge-request (run)
  (let ((mr-id (gitlab-merge-request-iid run)))
    (when mr-id
      (let* ((repo (channel-repo (recorder-run-channel run))))
        (let ((project-path (project-path repo))
              (mr-id (safe-get-mr-id run)))
          (when mr-id
            (let ((res (json:decode-json-from-string
                        (gitlab-request repo
                                        (format nil "/projects/~a/merge_requests/~a"
                                                (urlencode:urlencode project-path)
                                                mr-id)))))
              (make-instance 'merge-request
                              :base-sha (assoc-value (assoc-value res :diff--refs) :base--sha )))))))))


(defmethod maybe-promote ((promoter merge-request-promoter) run)
  (restart-case
      (cond
        ((typep (channel-repo (recorder-run-channel run))
                'gitlab-repo)
         (let* ((mr (get-merge-request run)))
           (when mr
             (maybe-promote-mr promoter run mr))))
        (t
         (log:info "Not promoting, gitlab")))
    (restart-maybe-promote ()
      (maybe-promote promoter run))))

(defun comment (promoter message)
  (push message (comments promoter)))

(defclass gitlab-acceptable (base-acceptable)
  ((report :initarg :report
           :accessor acceptable-report)
   (discussion-id :accessor discussion-id))
  (:metaclass bknr.datastore:persistent-class))

(defmethod (setf acceptable-state) :before (state (acceptable gitlab-acceptable))
  (flet ((not-null! (x) (assert x) x))
   (let* ((run (report-run (acceptable-report acceptable)))
          (repo (channel-repo (recorder-run-channel run))))
    (let ((res (gitlab-request repo
                               (format nil "/projects/~a/merge_requests/~a/discussions/~a"
                                       (urlencode:urlencode (project-path repo))
                                       (safe-get-mr-id run)
                                       (discussion-id acceptable))
                               :method :put
                               :content `(("resolved" . ,(ecase state
                                                           (:accepted
                                                            "true")
                                                           (:rejected
                                                            "false")))))))
      state))))

(defun maybe-promote-mr (promoter run mr)
  (let* ((channel (recorder-run-channel run))
         (base-sha (base-sha mr))
         (base-run (production-run-for channel
                                       :commit base-sha)))
    (cond
      ((not base-run)
       (comment promoter "Parent commit not available on master to generate Screenshot report, try rebasing or rerunning"))
      (t
       (let ((diff-report (make-diff-report run base-run)))
         (cond
           ((diff-report-empty-p diff-report)
            (comment promoter "No screenshot changes"))
           (t
            (let ((report (make-instance 'report
                                         :run run
                                         :previous-run base-run
                                         :channel (when run (recorder-run-channel run))
                                         :title (diff-report-title diff-report))))
              (with-transaction ()
                (setf (report-acceptable report)
                      (make-instance 'gitlab-acceptable
                                     :report report)))
              (setf (promoter-report promoter)
                    report)
              (comment promoter
                (format
                 nil
                 "Screenshot changes: ~a ~a/report/~a"
                 (diff-report-title diff-report)
                 (installation-domain (installation))
                 (util:oid report)))))))))))

(defun comment-now (promoter run comment)
  (let* ((channel (recorder-run-channel run))
         (repo (channel-repo channel))
         (mr-id (safe-get-mr-id run)))
    (let ((res (gitlab-request repo
                               (format nil "/projects/~a/merge_requests/~a/discussions"
                                       (urlencode:urlencode (project-path repo))
                                       mr-id)
                               :method :post
                               :content `(("body" . ,comment)))))
      (assoc-value (json:decode-json-from-string res) :id))))

(defmethod maybe-send-tasks ((promoter merge-request-promoter) run)
  (restart-case
      (let ((report (promoter-report promoter )))
        (loop for comment in (comments promoter)
              do
                 (let ((disc-id (comment-now promoter run comment)))
                   (with-transaction ()
                     (setf (discussion-id (report-acceptable report))
                           disc-id)))))
    (retry-maybe-send-tasks ()
      (maybe-send-tasks promoter run))))


(register-promoter 'merge-request-promoter)
