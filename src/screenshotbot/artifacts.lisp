;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/artifacts
  (:use :cl)
  (:import-from #:bknr.datastore
                #:blob
                #:persistent-class)
  (:import-from #:bknr.indices
                #:unique-index)
  (:import-from #:core/installation/installation
                #:installation-domain)
  (:import-from #:easy-macros
                #:def-easy-macro)
  (:import-from #:hunchentoot-extensions
                #:make-url)
  (:import-from #:screenshotbot/installation
                #:installation)
  (:import-from #:screenshotbot/server
                #:defhandler)
  (:import-from #:util/store/store
                #:object-store)
  (:export
   #:artifact
   #:artifact-link
   #:artifact-with-name
   #:def-artifact-hook
   #:md5-hex)
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

(defvar *artifact-hooks* nil)

(defclass artifact-hook ()
  ((dep :initarg :dep
        :reader artifact-hook-dep)
   (callback :initarg :callback
             :reader artifact-hook-callback)))

(defclass generated-artifact ()
  ((key :initarg :key
        :reader generated-artifact-key)
   (name :initarg :name
         :reader generated-artifact-name)
   (deps :initarg :deps
         :reader generated-artifact-deps)
   (generator :initarg :generator
              :reader generated-artifact-generator)))

(defvar *in-test-p* nil)

(defun md5-hex (f)
  (ironclad:byte-array-to-hex-string (md5:md5sum-file f)))

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
    (let ((util.cdn:*cdn-cache-key*
            (ignore-errors
             (file-write-date file-name))))
      (let ((url (make-url 'artifact-get :name name)))
        (cond
          (cdn
           (util.cdn:make-cdn url))
          (t
           (format nil "~a~a"
                   (installation-domain (installation))
                   url)))))))

(def-easy-macro def-artifact-hook (key artifact-name &fn fn)
  (setf
   (alexandria:assoc-value *artifact-hooks* key :test #'equal)
   (make-instance 'artifact-hook
                   :dep artifact-name
                   :callback fn)))

(defun call-hooks (dep)
  (loop for (nil . hook) in *artifact-hooks*
        if (equal dep (artifact-hook-dep hook))
          do
        (funcall (artifact-hook-callback hook))))
