(defpackage :screenshotbot/sdk/cli-common
  (:use #:cl)
  (:import-from #:easy-macros
                #:def-easy-macro)
  (:import-from #:clingon.options
                #:make-option)
  (:import-from #:clingon.command
                #:getopt)
  (:import-from #:screenshotbot/sdk/hostname
                #:api-hostname)
  (:import-from #:screenshotbot/sdk/api-context
                #:json-api-context
                #:api-context)
  (:import-from #:alexandria
                #:assoc-value)
  (:import-from #:util/health-check
                #:run-health-checks)
  (:import-from #:screenshotbot/sdk/install
                #:credential-file)
  (:import-from #:screenshotbot/api/model
                #:decode-json)
  (:import-from #:screenshotbot/sdk/fetch-run
                #:save-run)
  (:import-from #:screenshotbot/sdk/clingon-api-context
                #:with-clingon-api-context)
  (:import-from #:screenshotbot/sdk/upload-commit-graph
                #:upload-commit-graph/command)
  (:import-from #:screenshotbot/sdk/batch
                #:batch/command)
  (:export
   #:with-clingon-api-context
   #:common-run-options
   #:register-root-command
   #:dev/command))
(in-package :screenshotbot/sdk/cli-common)

(declaim (ftype (function) dev/command))

(defvar *root-commands* nil)

(defun root/handler (cmd)
  (clingon:print-usage-and-exit cmd t))

(defclass root-command (clingon:command)
  ())

(defmethod clingon:command-usage-string ((self root-command))
  "recorder ci record --directory /path/to/screenshots --channel channel-name
  recorder dev [record|verify] <options>")

(defun root-options ()
  (list
   (make-option
    :string
    :long-name "api-key"
    :initial-value nil
    :description "The Screenshotbot API Key"
    :env-vars (list "SCREENSHOTBOT_API_KEY")
    :key :api-key)
   (make-option
    :string
    :long-name "api-secret"
    :initial-value nil
    :key :api-secret
    :env-vars (list "SCREENSHOTBOT_API_SECRET")
    :description "The Screenshotbot API Secret")
   (make-option
    :string
    :key :hostname
    :long-name "api-hostname"
    :initial-value "https://api.screenshotbot.io"
    :env-vars (list "SCREENSHOTBOT_API_HOSTNAME")
    :description "The API hostname used as an endpoint. You will need to supply this if using this in the OSS version of Screenshotbot, or if you're an Enterprise customer with a dedicated installation.")
   (make-option
    :boolean
    :key :verbose
    :long-name "verbose"
    :description "Verbose logging")))

(defun download-run/command ()
  (clingon:make-command
   :name "download-run"
   :handler (lambda (cmd)
              (with-clingon-api-context (api-context cmd)
                (save-run
                 api-context
                 (getopt cmd :run-id)
                 :output
                 (format nil "~a/" (or
                                    (getopt cmd :output)
                                    (format nil "./~a" (getopt cmd :run-id))))
                 :channel (getopt cmd :channel)
                 :branch (getopt cmd :branch))))
   :description "Use this to download a run and all of its images locally."
   :options (list
             (make-option
              :string
              :long-name "id"
              :initial-value nil
              :description "The ID of the run, this is the ID you see in https://screenshotbot.io/runs/<ID>. Be aware that you cannot use a report ID here."
              :key :run-id)
             (make-option
              :string
              :long-name "channel"
              :initial-value nil
              :key :channel
              :description "A channel name, to provide instead of the --id, which will be used to download the active channel")
             (make-option
              :string
              :long-name "branch"
              :initial-value nil
              :key :branch
              :description "The branch to disambiguate active runs on the channel. By default, we'll pick a branch that matches `main` or `master`.")
             (make-option
              :string
              :long-name "output"
              :initial-value nil
              :description "The output directory to save the images. If not present it will default to  ./<id>"
              :key :output))))


(defun root/command ()
  (make-instance
   'root-command
   :name "recorder"
   :handler #'root/handler
   :description "Use this script from your CI pipelines or locally to
upload screenshots and generate reports with Screenshotbot.

This is the documentation for the experimental V2 of the CLI interface. To
view the documentation for the stable interface, run `recorder --help`
as opposed to `recorder help`."
   :options (root-options)
   :sub-commands
   (list
    (self-test/command)
    (ci/command)
    (dev/command)
    (download-run/command)
    (batch/command))))


(defun ci/command ()
  (clingon:make-command
   :name "ci"
   :handler (lambda (cmd)
              (clingon:print-usage-and-exit cmd t))
   :description "Collection of commands that are typically run during CI jobs. In particular, `ci record` might be what you're looking for."
   :sub-commands (list*
                  (upload-commit-graph/command)
                  (mapcar #'funcall (mapcar #'cdr *root-commands*)))))

(defun common-run-options ()
  "A list of run options that are common between directory runs and static-website runs."
  (list
   (make-option
    :flag
    :long-name "production"
    :initial-value :true
    :description "Treat this as a production run on CI. (As opposed to a developer running a local run. This avoids pollution of the main history.)"
    :key :production)
   (make-option
    :string
    :long-name "repo-url"
    :initial-value nil
    :description "The URL of the repository (e.g. 'https://github.com/foo/bar')"
    :key :repo-url)
   (make-option
    :string
    :long-name "channel"
    :initial-value "unnamed-channel"
    :description "The channel name for this run"
    :key :channel)
   (make-option
    :string
    :long-name "main-branch"
    :initial-value nil
    :description "The main branch to compare this run with. e.g. `main`, `master`, or `trunk`. The default is to first try `main`, and then `master` and pick the first such that origin/<name> exists."
    :key :main-branch)
   (make-option
    :string
    :long-name "pull-request"
    :initial-value nil
    :description "The pull request URL."
    :key :pull-request)))

(defmacro register-root-command (fn)
  `(setf (assoc-value *root-commands* ,fn) ,fn))


(defun self-test/command ()
  (clingon:make-command
   :name "self-test"
   :description "Run a few diagnostic self-tests. This can be useful to figure out why this tool is failing in your environment."
   :handler (lambda (cmd)
              (uiop:quit (if (run-health-checks) 0 1)))))
