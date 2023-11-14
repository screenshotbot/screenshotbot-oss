;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/sdk/install
  (:use #:cl)
  (:import-from #:util/json-mop
                #:ext-json-serializable-class)
  (:import-from #:screenshotbot/api/model
                #:installation-url
                #:encode-json)
  (:import-from #:screenshotbot/sdk/api-context
                #:json-api-context
                #:api-context
                #:fetch-version)
  (:import-from #:screenshotbot/sdk/sdk
                #:request)
  (:import-from #:clingon.options
                #:make-option)
  (:import-from #:clingon.command
                #:getopt)
  (:import-from #:screenshotbot/sdk/sentry
                #:with-sentry)
  (:export
   #:install-credentials
   #:install/command))
(in-package :screenshotbot/sdk/install)

(defun fix-hostname (hostname)
  (multiple-value-bind (number version)
      (fetch-version (make-instance 'api-context :hostname hostname))
    (declare (ignore number))
    (installation-url version)))

(defun install-credentials (hostname &key no-stdin token)
  (let ((hostname (fix-hostname hostname)))
    (cond
      ((not (str:emptyp token))
       (install-pasted hostname token))
      (t
       (format t "LOGIN
Open this page in your browser and log in if necessary:

~a

Then paste the Token on that page below." (quri:merge-uris "/api-keys/cli" hostname))
       (finish-output t)

       (unless no-stdin
         (flet ((prompt ()
                  (format t "

    Paste Token from that page: ")
                  (finish-output t)))
           (prompt)
           (loop for line = (str:trim (read-line))
                 while (str:emptyp line)
                 do (prompt)
                 finally
                    (install-pasted hostname line)
                    (format t "~%Your credentials are now installed~%~%"))))))))


(defun credential-file ()
  (ensure-directories-exist
   (pathname "~/.config/screenshotbot/credentials")))

(defun install-pasted (hostname line)
  (let ((file (credential-file)))
    (with-open-file (stream file :direction :output :if-exists :supersede)
      (destructuring-bind (key secret) (str:split ":" (str:trim line))
        (let ((api-ctx (make-instance 'json-api-context
                         :hostname hostname
                         :key key
                         :secret secret)))
          (request
           api-ctx
           "/api/finalize-cli"
           :method :post
           :parameters `(("hostname" . ,(uiop:hostname))))
          (write-string
           (encode-json
            api-ctx)
           stream))))))


(defun install/command ()
  (clingon:make-command
   :name "install"
   :description "Installs API keys to connect to your Screenshotbot server"
   :options (list
             (make-option
              :string
              :long-name "token"
              :initial-value nil
              :description "A token to install non-interactively"
              :key :token)
             (make-option
              :flag
              :long-name "no-stdin"
              :initial-value nil
              :description "[internal use only]"
              :key :no-stdin))
   :handler (lambda (cmd)
              (with-sentry ()
                (install-credentials (getopt cmd :hostname)
                                     :no-stdin (getopt cmd :no-stdin)
                                     :token (getopt cmd :token))))))
