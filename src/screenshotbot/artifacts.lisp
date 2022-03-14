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
  (:export #:artifact-with-name
           #:artifact-link
           #:md5-hex
           #:artifact)
  (:local-nicknames (#:dns-client #:org.shirakumo.dns-client)))
(in-package :screenshotbot/artifacts)

(defclass artifact (blob)
  ((name :initarg :name
         :index-initargs (:test #'equal)
         :index-type unique-index
         :index-reader artifact-with-name
         :accessor artifact-name))
  (:metaclass persistent-class))


(defun md5-hex (f)
  (ironclad:byte-array-to-hex-string (md5:md5sum-file f)))


(let ((lock (bt:make-lock)))
  (defhandler (nil :uri "/intern/artifact/upload" :method :put) (name hash upload-key)
    (assert name)
    (assert (equal (dns-client:resolve "tdrhq.com") (hunchentoot:real-remote-addr)))
    (assert (secret :artifact-upload-key))
    (assert (equal upload-key (secret :artifact-upload-key)))
    (let ((artifact (bt:with-lock-held (lock)
                      (or
                       (artifact-with-name name)
                       (make-instance 'artifact
                                       :name name)))))
      (let ((input-file (pathname "~/tmp-upload")))
       (let ((blob-pathname (bknr.datastore:blob-pathname artifact)))
         (assert (equal
                  (md5-hex input-file)
                  hash))
         (uiop:rename-file-overwriting-target
          input-file blob-pathname)
         (assert (equal
                  (md5-hex blob-pathname)
                  hash))))
      "OK")))


(defhandler (artifact-get :uri "/artifact/:name") (name cache-key)
  (declare (ignore cache-key))
  (let ((name (car (str:rsplit "." name :limit 2))))
    (let* ((artifact (artifact-with-name name))
           (file (bknr.datastore:blob-pathname artifact)))
      (assert (path:-e file))
      (hunchentoot:handle-static-file file))))

(defmethod artifact-link ((name string) &key (cdn t))
  (let ((artifact (artifact-with-name (str:downcase name))))
    (let ((util.cdn:*cdn-cache-key* (file-write-date (bknr.datastore:blob-pathname artifact))))
      (let ((url (make-url 'artifact-get :name name)))
        (cond
          (*delivered-image*
           (format nil "~a~a" (installation-domain (installation)) url))
          (cdn
           (util.cdn:make-cdn url))
          (t url))))))

;; (upload-artifact "installer-linux" "/home/arnold/.cache/common-lisp/lw-7.1.2-linux-x64/home/arnold/builds/web/screenshotbot/sdk/installer")
