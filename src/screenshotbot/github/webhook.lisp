;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(uiop:define-package :screenshotbot/github/webhook
  (:use #:cl #:alexandria)
  (:import-from #:bknr.datastore
                #:store-object
                #:persistent-class
                #:hash-index
                #:with-transaction)
  (:import-from #:screenshotbot/model/channel
                #:github-get-canonical-repo)
  (:import-from #:screenshotbot/server
                #:defhandler)
  (:import-from #:screenshotbot/model/company
                #:installation-id)
  (:import-from #:screenshotbot/model/recorder-run
                #:github-repo)
  (:import-from #:screenshotbot/github/plugin
                #:webhook-secret
                #:github-plugin)
  (:import-from #:util/threading
                #:with-extras
                #:make-thread
                #:max-pool
                #:ignore-and-log-errors)
  (:import-from #:util/request
                #:http-request)
  (:import-from #:util/store/store-migrations
                #:def-store-migration)
  (:export
   #:pull-request
   #:github-get-canonical-repo
   #:repo-full-name
   #:pull-request-id
   #:pull-request-head
   #:pull-request-base
   #:all-pull-requests
   #:pull-request-with-url
   #:*hooks*))
(in-package :screenshotbot/github/webhook)

(defmethod github-get-canonical-repo (repo)
  (let ((host (if (str:containsp "bitbucket" repo)
                  "bitbucket.org"
                  "github.com")))
   (cl-ppcre:regex-replace-all
    (format nil "^(ssh://)?git@~a[:/]" host)
    (cl-ppcre:regex-replace-all
     "https://api."
     (cl-ppcre:regex-replace-all "[.]git$"
                                 (cl-ppcre:regex-replace-all "^git://"
                                  repo "https://")
                                 "")
     "https://")
    (format nil "https://~a/" host))))

(defclass pull-request (store-object)
  ((url
    :initarg :url
    :index-type hash-index
    :index-initargs (:test #'equal)
    :index-reader pull-requests-with-url
    :index-values all-pull-requests)
   (pull-id
    :initarg :pull-id
    :initform nil
    :accessor pull-request-id)
   (repo-full-name
    :initarg :repo-full-name
    :accessor repo-full-name)
   (head
    :initarg :head
    :accessor pull-request-head)
   (base
    :initarg :base
    :accessor pull-request-base))
  (:metaclass persistent-class)
  (:documentation "DEPRECATED: we don't believe this was used, so we removed all the code
creating and referencing this. TODO: delete existing objects, that only show up in prod."))

(defun pull-request-with-url (url)
  (error "No longer supported"))

(defun channels-for-pull-request (pull-request))

(defvar *hooks* nil)

(defvar *thread-pool* (make-instance 'max-pool
                                     :max 20))

(defhandler (nil :uri "/github-webhook") ()
  (let* ((plugin (github-plugin))
         (webhook-secret (webhook-secret plugin)))
    (let ((stream (hunchentoot:raw-post-data
                   :want-stream t
                   :force-binary t))
          (length (parse-integer (hunchentoot:header-in* :content-length)))
          (signature (hunchentoot:header-in* :x-hub-signature-256)))
      (let ((data (make-array length :element-type 'flexi-streams:octet )))
        (read-sequence data stream)
        (make-thread
         (lambda ()
           (ignore-and-log-errors ()
             (validate-hmac :webhook-secret webhook-secret
                            :data data
                            :signature signature)
             (let ((json (json:decode-json
                          (flexi-streams:make-flexi-stream
                           (flexi-streams:make-in-memory-input-stream data)))))
               (log:debug "got json: ~a" json)
               ;; todo: does this next call actually do anything? We used to
               ;; use it before when we did special code for Pull Requests,
               ;; and eventually just moved to the checks API. I think it
               ;; could go.
               (loop for hook in *hooks*
                     do (funcall hook json)))))
         :name "github-webhook"
         :pool *thread-pool*))
      "OK")))

(defun validate-hmac (&key webhook-secret
                        data
                        signature)
  (let ((hmac (ironclad:make-hmac (flexi-streams:string-to-octets
                                   webhook-secret)
                                  :sha256)))
    (ironclad:update-hmac hmac data)
    (let* ((expected (ironclad:hmac-digest hmac))
           (actual signature)
           (expected (format nil "sha256=~a"
                             (ironclad:byte-array-to-hex-string expected))))
      (unless (equal expected actual)
        (with-extras (("data" data)
                      ("decoded-data"(flex:octets-to-string data :external-format :utf-8)))
         (error "invalid hmac, expected ~a, got ~a" expected actual)))))
  (log:debug "hmac validated"))


(def-store-migration ("Delete pull-request objects -- T1966" :version 35)
  (mapc #'bknr.datastore:delete-object
        (bknr.datastore:class-instances 'pull-request)))
