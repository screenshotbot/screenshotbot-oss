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
  (:export #:phabricator-plugin))
(in-package :screenshotbot/phabricator/plugin)

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
   (or
    (assoc-value *cache* company)
    (setf (assoc-value *cache* company)
          (assoc-value
           (assoc-value
            (call-conduit  phab
                           "diffusion.repository.search"
                           `())
            :result)
           :data)))))

(cl-cron:make-cron-job (lambda ()
                         (bt:with-lock-held (*lock*)
                          (setf *cache* nil)))
                       :step-min 30
                       :hash-key 'clean-phabricator-cache)

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

(defmethod commit-link ((repo phabricator-git-repo) hash)
  (let* ((company (company repo))
         (config (phabricator-config-for-company company))
         (phabricator-url (phabricator-url config))
         (conduit-api-key (conduit-api-key config)))
    (when (and phabricator-url
               conduit-api-key)
     (let* ((phab-instance (make-instance 'phab-instance
                                           :url phabricator-url
                                           :api-key conduit-api-key))
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
           (t "#")))))))
