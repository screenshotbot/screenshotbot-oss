;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(uiop:define-package :screenshotbot/github/plugin
  (:use #:cl #:alexandria)
  (:import-from #:screenshotbot/plugin
                #:plugin
                #:plugin-parse-repo)
  (:import-from #:screenshotbot/model/recorder-run
                #:github-repo)
  (:import-from #:screenshotbot/installation
                #:find-plugin
                #:installation)
  (:export
   #:github-plugin
   ;; todo: separate these to
   #:github-repo
   #:app-id
   #:private-key
   #:app-name
   #:webhook-secret
   #:webhook-relays
   #:verification-oauth-provider))
(in-package :screenshotbot/github/plugin)

(defclass github-plugin (plugin)
  ((app-name :initarg :app-name
             :accessor app-name)
   (webhook-secret :initarg :webhook-secret
                   :accessor webhook-secret)
   (app-id :initarg :app-id
           :accessor app-id)
   (private-key :initarg :private-key
                :accessor private-key)
   (%verified-orgs :initarg :verified-orgs
                   :initform nil
                   :reader verified-orgs
                   :documentation "A list of orgs for which we don't have to do the manual OAuth verification. If T, then all orgs will be automatically verified. This is reasonable if you have control
over the instance and it's only used by people you know.")
   (webhook-relays :initarg :webhook-relays
                   :initform nil
                   :accessor webhook-relays
                   :documentation "A list of backends to relay this webhook to.")
   (verification-oauth-provider
    :initarg :verification-oauth-provider
    :accessor verification-oauth-provider
    :documentation "Similar to the login OAuth for GitHub, it's only used as an authorization to verify that the user can access the specific repo.")))

(defmethod initialize-instance :after ((plugin github-plugin)
                                       &key private-key-file &allow-other-keys)
  (when private-key-file
    (setf (private-key plugin)
          (uiop:read-file-string private-key-file))))

(let ((cache nil)
      (lock (bt:make-lock)))
  (defun make-github-repo (&key link company)
    (bt:with-lock-held (lock)
      (symbol-macrolet ((place (assoc-value cache (cons link company) :test 'equal)))
        (or place
            (setf place (make-instance 'github-repo :link link
                                                    :company company)))))))

(defmethod plugin-parse-repo ((plugin github-plugin)
                              company
                              repo-str)
  (log:debug "Parsing repo for company ~a" company)
  (when (str:containsp "github" repo-str)
    (make-github-repo :link repo-str
                      :company company)))

(defun github-plugin (&key (installation (installation)))
  (find-plugin installation 'github-plugin))
