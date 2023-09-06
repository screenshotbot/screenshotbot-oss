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
                #:encode-json)
  (:export
   #:dev/command)
  (:local-nicknames (#:run-context #:screenshotbot/sdk/run-context)))
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
  (log:info "here")
  (let ((ctx (make-instance 'dev-run-context
                            :productionp nil
                            :channel channel
                            :main-branch "main")))
    (with-flags-from-run-context (ctx)
      (log:info "hello")
      (make-directory-run
       api-ctx
       (make-instance 'image-directory
                      :directory directory)
       :channel channel
       :repo (make-instance 'null-repo)))))

#+nil
(%make-run-and-get-id (make-instance 'api-context
                                     :key "DD9RA2F8ZXBAUTRZ6T4D"
                                     :hostname "https://api.screenshotbot.io"
                                     :secret "UY8JzaybHIDEyD7SzBEqqqEPY2mNiH7W113Rum6h")
                      :directory "/home/arnold/builds/fast-example/screenshots/"
                      :channel "dummy")

(defun make-run-and-get-id (cmd)
  (when (getopt cmd :verbose)
    (log:config :debug))
  (with-clingon-api-context (api-ctx cmd)
    (%make-run-and-get-id
     api-ctx
     :channel (getopt cmd :channel)
     :directory (getopt cmd :directory))))

(defun recording-file (channel)
  (ensure-directories-exist
   (path:catfile
    (path:-d (uiop:getenv "HOME"))
    ".config/screenshotbot/recordings/"
    (format nil "~a.json" channel))))

(defun record/command ()
  (clingon:make-command
   :name "record"
   :description "Record a user run"
   :options (default-options)
   :handler (lambda (cmd)
              (with-sentry ()
                (let ((run (make-run-and-get-id cmd)))
                  (let ((recording (recording-file (getopt cmd :channel))))
                    (uiop:with-staging-pathname (recording)
                      (with-open-file (stream recording :direction :output
                                                        :if-exists :supersede)
                        (write-string (encode-json run) stream)))))))))

(defun verify/command ()
  (clingon:make-command
   :name "verify"
   :description "Verify a run against the last recorded run"
   :options (default-options)
   :handler (lambda (cmd)
              (with-sentry ()
               (let ((run-id (make-run-and-get-id cmd)))
                 (error "unimpl"))))))

(defun dev/command ()
  (clingon:make-command
   :name "dev"
   :description "Tools that are run from a developer device (as opposed to CI)"
   :sub-commands
   (list
    (record/command)
    (verify/command))))
