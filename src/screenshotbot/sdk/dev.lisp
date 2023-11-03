;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/sdk/dev
  (:use #:cl)
  (:import-from #:screenshotbot/sdk/run-context
                #:with-flags-from-run-context
                #:productionp
                #:env-reader-run-context
                #:run-context)
  (:import-from #:clingon.options
                #:make-option)
  (:import-from #:screenshotbot/sdk/sdk
                #:request
                #:make-directory-run)
  (:import-from #:screenshotbot/sdk/cli-common
                #:with-clingon-api-context
                #:dev/command)
  (:import-from #:clingon.command
                #:getopt)
  (:import-from #:screenshotbot/sdk/env
                #:env-reader)
  (:import-from #:screenshotbot/sdk/bundle
                #:image-directory)
  (:import-from #:screenshotbot/sdk/sentry
                #:with-sentry)
  (:import-from #:screenshotbot/sdk/git
                #:null-repo)
  (:import-from #:screenshotbot/sdk/api-context
                #:api-context)
  (:import-from #:screenshotbot/api/model
                #:decode-json
                #:encode-json)
  (:import-from #:alexandria
                #:assoc-value)
  (:export
   #:dev/command)
  (:local-nicknames (#:run-context #:screenshotbot/sdk/run-context)
                    (#:dto #:screenshotbot/api/model)))
(in-package :screenshotbot/sdk/dev)

(defclass dev-run-context (run-context
                           env-reader-run-context)
  ()
  (:default-initargs :env (make-instance 'env-reader)))

(defun default-options ()
  ;; TODO: recursive
  (list
   (make-option
    :string
    :long-name "channel"
    :initial-value "default-dev-channel"
    :description "The channel name to use. We save recordings on a per channel basis"
    :key :channel)
   (make-option
    :string
    :long-name "directory"
    :description "The directory with screenshots to record"
    :initial-value nil
    :required t
    :key :directory)))

(defmethod productionp ((self dev-run-context))
  nil)

(defun %make-run-and-get-id (api-ctx &key directory channel)
  (let ((ctx (make-instance 'dev-run-context
                            :productionp nil
                            :channel channel
                            :main-branch "main")))
    (with-flags-from-run-context (ctx)
      (make-directory-run
       api-ctx
       (make-instance 'image-directory
                      :directory directory)
       :channel channel
       :repo (make-instance 'null-repo)))))

(defun make-run-and-get-id (cmd)
  (when (getopt cmd :verbose)
    (log:config :debug))
  (with-clingon-api-context (api-ctx cmd)
    (%make-run-and-get-id
     api-ctx
     :channel (getopt cmd :channel)
     :directory (getopt cmd :directory))))

(defun homedir ()
  (path:-d (uiop:getenv "HOME")))

(defun recording-file (channel)
  (ensure-directories-exist
   (path:catfile
    (homedir)
    ".config/screenshotbot/recordings/"
    (format nil "~a.json" channel))))

(defun record-run (run channel)
  (let ((recording (recording-file channel)))
    (uiop:with-staging-pathname (recording)
      (with-open-file (stream recording :direction :output
                                        :if-exists :supersede)
        (write-string (encode-json run) stream)))))

(defun record/command ()
  (clingon:make-command
   :name "record"
   :description "Record a user run"
   :options (default-options)
   :handler (lambda (cmd)
              (with-sentry ()
                (let ((run (make-run-and-get-id cmd)))
                  (record-run run (getopt cmd :channel)))))))

(defun read-recorded-run (channel)
  (let ((file (recording-file channel)))
    (cond
      ((path:-e file)
       (decode-json (uiop:read-file-string file)
                    'dto:run))
      (t
       (error "No recording for channel ~a" channel)))))

(defun compare-runs (api-context run1 run2)
  (let ((body
          (request api-context (format nil "/api/run/~a/compare/~a"
                                       (dto:run-id run1)
                                       (dto:run-id run2)))))
    (make-instance 'dto:comparison
                   :samep (assoc-value body :samep)
                   :title (assoc-value body :title)
                   :url (assoc-value body :url))))

(defun verify/command ()
  (clingon:make-command
   :name "verify"
   :description "Verify a run against the last recorded run"
   :options (default-options)
   :handler (lambda (cmd)
              (with-sentry ()
                (let ((recorded-run (read-recorded-run (getopt cmd :channel))))
                  (with-clingon-api-context (api-ctx cmd)
                   (let* ((run (make-run-and-get-id cmd))
                          (comparison (compare-runs api-ctx run recorded-run)))
                     (cond
                       ((dto:comparison-samep comparison)
                        (log:info "Nothing's changed.")
                        (uiop:quit 0))
                       (t
                        (log:info "~a" (dto:comparison-title comparison))
                        (log:info "See changes at ~a" (dto:comparison-url comparison))
                        (uiop:quit 1))))))))))

(defun dev/command ()
  (clingon:make-command
   :name "dev"
   :description "Tools that are run from a developer device (as opposed to CI)"
   :handler (lambda (cmd)
              (clingon:print-usage-and-exit cmd t))
   :sub-commands
   (list
    (record/command)
    (verify/command))))
