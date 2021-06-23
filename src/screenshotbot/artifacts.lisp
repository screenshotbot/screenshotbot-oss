;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(pkg:define-package :screenshotbot/artifacts
    (:use #:cl
          #:alexandria)
  (:import-from #:./server
                #:*domain*
                #:defhandler)
  (:import-from #:util
                #:make-url
                #:*delivered-image*)
  (:import-from #:bknr.datastore
                #:persistent-class
                #:unique-index
                #:blob)
  (:export #:artifact-with-name
           #:artifact-link
           #:artifact))

(defclass artifact (blob)
  ((name :initarg :name
         :index-initargs (:test #'equal)
         :index-type unique-index
         :index-reader artifact-with-name
         :accessor artifact-name))
  (:metaclass persistent-class))

(let ((lock (bt:make-lock)))
  (defhandler (nil :uri "/intern/artifact/upload" :method :put) (name hash)
    (assert name)
    (assert (equal "73.15.166.71" (hunchentoot:real-remote-addr)))
    (let ((artifact (bt:with-lock-held (lock)
                      (or
                       (artifact-with-name name)
                       (make-instance 'artifact
                                       :name name)))))
      (let ((input-file (pathname "~/tmp-upload")))
       (let ((blob-pathname (bknr.datastore:blob-pathname artifact)))
         (assert (equal
                  (screenshotbot-utils:md5-hex input-file)
                  hash))
         (uiop:rename-file-overwriting-target
          input-file blob-pathname)
         (assert (equal
                  (screenshotbot-utils:md5-hex blob-pathname)
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
           (format nil "~a~a" *domain* url))
          (cdn
           (util.cdn:make-cdn url))
          (t url))))))

;; (upload-artifact "installer-linux" "/home/arnold/.cache/common-lisp/lw-7.1.2-linux-x64/home/arnold/builds/web/screenshotbot/sdk/installer")
