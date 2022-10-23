;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(uiop:define-package :screenshotbot/phabricator/plugin
  (:use #:cl
        #:alexandria
        #:util/phabricator/conduit)
  (:import-from #:screenshotbot/plugin
                #:plugin
                #:plugin-parse-repo)
  (:import-from #:screenshotbot/git-repo
                #:company
                #:repo-link
                #:commit-link
                #:generic-git-repo)
  (:import-from #:screenshotbot/model/company
                #:phabricator-config-for-company
                #:phabricator-url
                #:conduit-api-key)
  (:import-from #:util/cron
                #:def-cron)
  (:import-from #:screenshotbot/dashboard/review-link
                #:review-link-impl)
  (:import-from #:screenshotbot/model/recorder-run
                #:recorder-run
                #:phabricator-diff-id)
  (:import-from #:util/misc
                #:?.)
  (:export #:phabricator-plugin
           #:phab-instance-for-company)
  (:local-nicknames (#:a #:alexandria)))
(in-package :screenshotbot/phabricator/plugin)

(markup:enable-reader)

(defclass phabricator-plugin (plugin)
  ())

(defclass phabricator-git-repo (generic-git-repo)
  ())

(defmethod plugin-parse-repo ((plugin phabricator-plugin)
                              company
                              repo-str)
  (when (str:containsp "phabricator" repo-str)
    (make-instance 'phabricator-git-repo :link repo-str
                                         :company company)))

(defparameter *cache* nil)
(defvar *lock* (bt:make-lock))

(defmethod get-repos (company (phab phab-instance))
  "Get the parsed JSON information about all the repos with
diffusion.repository.search."
  ;; todo: paging. Currently this is limited to phabricator instances
  ;; with less than 100 repos.
  (bt:with-lock-held (*lock*)
    (let ((assoc (assoc company *cache*)))
      (if assoc
          (cdr assoc)
          (setf (assoc-value *cache* company)
                ;; Ignore errors, because the Phabricator instance
                ;; might be down. TODO: if this happens we should
                ;; reset the cache at some point
                (ignore-errors
                 (assoc-value
                  (assoc-value
                   (call-conduit  phab
                                  "diffusion.repository.search"
                                  `())
                   :result)
                  :data)))))))

(def-cron clean-phabricator-cache (:step-min 30)
  (bt:with-lock-held (*lock*)
    (setf *cache* nil)))

(defmethod find-repo-by-name (repos (name string))
  "Find the repo json from diffusion.repository.search that
corresponds to this name. name could be shortName or full-name."
  (loop for x in repos
        for fields = (assoc-value x :fields)
        if (or
            (equal name (assoc-value fields :name))
            (equal name (assoc-value fields :short-name)))
          return x))

(defmethod get-callsign (repos name)
  "Given the repo name, and the list of repos (from get-repos), figure
out the callsign of the repo."
  (assoc-value
   (assoc-value
    (find-repo-by-name repos name)
    :fields)
   :callsign))

(defun phab-instance-for-company (company)
  (let* ((config (phabricator-config-for-company company))
         (phabricator-url (?. phabricator-url config))
         (conduit-api-key (?. conduit-api-key config)))
    (when (and phabricator-url
               conduit-api-key)
     (make-instance 'phab-instance
                    :url phabricator-url
                    :api-key conduit-api-key))))

(defmethod commit-link ((repo phabricator-git-repo) hash)
  (let* ((company (company repo))
         (phabricator-url (phabricator-url
                           (phabricator-config-for-company company))))
    (a:when-let* ((phab-instance (phab-instance-for-company company))
                  (repos (get-repos company phab-instance)))
      (multiple-value-bind (res parts)
          (cl-ppcre:scan-to-strings "/([^/]*)[.]git$" (repo-link repo))
        (declare (ignore res))
        (cond
          (res
           (let ((callsign (get-callsign repos (elt parts 0))))
             (if callsign
                 (format nil "~a/r~a~a" phabricator-url callsign hash)
                 "#")))
          (t "#"))))))


(defmethod review-link-impl ((repo phabricator-git-repo) (run recorder-run))
  (let* ((company (company repo))
         (config (phabricator-config-for-company company))
         (phabricator-url (phabricator-url config)))
    <a href= (format nil "~a/differential/diff/~a" phabricator-url
              (phabricator-diff-id run)) >
      Revision
    </a>))
