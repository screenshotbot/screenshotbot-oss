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
                #:encode-json)
  (:export
   #:install-credentials))
(in-package :screenshotbot/sdk/install)

(defun install-credentials (hostname)
  (format t "LOGIN
Open this page in your browser and log in if necessary:

~a

Then paste the Token on that page below.

    Paste Token from that page: " (quri:merge-uris "/api-keys/cli" hostname))
  (finish-output t)
  (let ((line (str:trim (read-line))))
    (install-pasted hostname line)
    (format t "~%Your credentials are now installed~%~%")))

(defclass credential ()
  ((hostname :initarg :hostname
             :json-key "hostname"
             :json-type :string)
   (api-key :initarg :key
            :json-key "apiKey"
            :json-type :string)
   (api-secret :initarg :secret
               :json-key "apiSecretKey"
               :json-type :string))
  (:metaclass ext-json-serializable-class))

(defun credential-file ()
  (ensure-directories-exist
   (pathname "~/.config/screenshotbot/credentials")))

(defun install-pasted (hostname line)
  (let ((file (credential-file)))
    (with-open-file (stream file :direction :output :if-exists :supersede)
      (destructuring-bind (key secret) (str:split ":" (str:trim line))
        (write-string
         (encode-json
          (make-instance 'credential
                         :hostname hostname
                         :key key
                         :secret secret))
         stream)))))
