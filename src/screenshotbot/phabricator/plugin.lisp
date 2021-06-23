;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(pkg:define-package :screenshotbot/phabricator/plugin
    (:use #:cl
          #:alexandria)
  (:import-from #:../plugin
                #:plugin
                #:plugin-parse-repo)
  (:import-from #:../git-repo
                #:repo-link
                #:generic-git-repo)
  (:export #:phabricator-plugin))

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


(defmethod commit-link ((repo phabricator-git-repo) hash)
  (multiple-value-bind (res parts)
      (cl-ppcre:scan-to-strings "/([^/]*)[.]git$" (repo-link repo))
    (declare (ignore res parts))
    (cond
      (res
       ;; todo: lots.
       (format nil "https://phabricator.tdrhq.com/rW~a"  hash))
      (t "#"))))
