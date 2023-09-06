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

(defun make-run-and-get-id (cmd)
  (let ((ctx (make-instance 'dev-run-context
                            :productionp nil)))
    (with-flags-from-run-context (ctx)
      (with-clingon-api-context (api-ctx cmd)
        (make-directory-run
         api-ctx
         (make-instance 'image-directory
                        :directory (getopt cmd :directory)))))))

(defun record/command ()
  (clingon:make-command
   :name "record"
   :description "Record a user run"
   :options (default-options)
   :handler (lambda (cmd)
              (with-sentry ()
               (let ((run-id (make-run-and-get-id cmd)))
                 (error "Unimplemented now ~a" run-id))))))

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
