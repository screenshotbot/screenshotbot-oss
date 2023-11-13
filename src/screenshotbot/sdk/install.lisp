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
  (:export
   #:install-credentials))
(in-package :screenshotbot/sdk/install)

(defun fix-hostname (hostname)
  (multiple-value-bind (number version)
      (fetch-version (make-instance 'api-context :hostname hostname))
    (declare (ignore number))
    (installation-url version)))

(defun install-credentials (hostname)
  (let ((hostname (fix-hostname hostname)))
    (format t "LOGIN
Open this page in your browser and log in if necessary:

~a

Then paste the Token on that page below.

    Paste Token from that page: " (quri:merge-uris "/api-keys/cli" hostname)))
  (finish-output t)
  (let ((line (str:trim (read-line))))
    (install-pasted hostname line)
    (format t "~%Your credentials are now installed~%~%")))

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
