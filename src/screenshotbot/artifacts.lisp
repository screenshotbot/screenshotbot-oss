;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(uiop:define-package :screenshotbot/artifacts
  (:use #:cl #:alexandria)
  (:import-from #:screenshotbot/server
                #:*domain*
                #:defhandler)
  (:import-from #:screenshotbot/secret
                #:defsecret
                #:secret)
  (:import-from #:util
                #:make-url
                #:*delivered-image*)
  (:import-from #:bknr.datastore
                #:persistent-class
                #:unique-index
                #:blob)
  (:import-from #:screenshotbot/installation
                #:installation
                #:installation-domain)
  (:import-from #:util/store
                #:object-store)
  (:import-from #:bknr.datastore
                #:store-directory)
  (:import-from #:bknr.datastore
                #:*store*)
  (:export #:artifact-with-name
           #:artifact-link
           #:md5-hex
           #:artifact)
  (:local-nicknames (#:dns-client #:org.shirakumo.dns-client)))
(in-package :screenshotbot/artifacts)

(defun artifacts-dir ()
  (ensure-directories-exist
   (path:catdir
    (object-store)
    "artifacts/")))

;; DEPRECATED: DO NOT USE.
(defclass artifact (blob)
  ((name :initarg :name
         :index-initargs (:test #'equal)
         :index-type unique-index
         :index-reader artifact-with-name
         :accessor artifact-name))
  (:metaclass persistent-class))

(defvar *in-test-p* nil)

(defun ensure-private-ip ()
  (unless *in-test-p* ;; can't mock because of multithreading
   (assert (equal (dns-client:resolve "tdrhq.com") (hunchentoot:real-remote-addr)))))

(defun md5-hex (f)
  (ironclad:byte-array-to-hex-string (md5:md5sum-file f)))

(defhandler (nil :uri "/intern/artifact/upload" :method :put) (name hash upload-key)
  (assert name)
  (assert (secret :artifact-upload-key))
  (ensure-private-ip)
  (assert (equal upload-key (secret :artifact-upload-key)))
  (let ((file-name (artifact-file-name name)))
    (hex:write-postdata-to-file file-name)
    (assert (equal
             (md5-hex file-name)
             hash))
    "OK"))


(defun artifact-file-name (name)
  (assert (not (str:containsp "/." name)))
  (assert (not (str:containsp ".." name)))
  (path:catfile (artifacts-dir)  (str:downcase name)))

(defhandler (artifact-get :uri "/artifact/:name") (name cache-key)
  (declare (ignore cache-key))
  (let ((file (artifact-file-name name)))
    (assert (path:-e file))
    (hunchentoot:handle-static-file file)))


(defmethod artifact-link ((name string) &key (cdn t))
  (let ((file-name (artifact-file-name name)))
    (let ((util.cdn:*cdn-cache-key* (file-write-date file-name)))
      (let ((url (make-url 'artifact-get :name name)))
        (cond
          (cdn
           (util.cdn:make-cdn url))
          (t url))))))

;; (upload-artifact "installer-linux" "/home/arnold/.cache/common-lisp/lw-7.1.2-linux-x64/home/arnold/builds/web/screenshotbot/sdk/installer")
