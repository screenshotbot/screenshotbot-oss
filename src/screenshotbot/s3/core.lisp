;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/s3/core
  (:use #:cl)
  (:import-from #:easy-macros
                #:def-easy-macro)
  (:import-from #:zs3
                #:secret-key
                #:access-key
                #:*credentials*
                #:*s3-endpoint*)
  (:import-from #:screenshotbot/installation
                #:null-s3-store)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:null-store))
(in-package :screenshotbot/s3/core)

(defclass s3-store ()
  ((endpoint :initarg :endpoint
             :reader endpoint)
   (bucket :initarg :bucket
           :reader bucket)
   (access-key :initarg :access-key
               :reader access-key)
   (secret-access-key :initarg :secret-access-key
                      :reader secret-key)))

(def-easy-macro with-store (store &fn fn)
  (let ((*s3-endpoint* (endpoint store))
        (*credentials* store))
    (funcall fn)))

(defmethod upload-file ((store s3-store) file key)
  (with-store (store)
    (zs3:put-file
     file
     (bucket store)
     key)))

(defmethod upload-file ((store null-s3-store) file key)
  nil)

#|
(upload-file *store* "/home/arnold/builds/web/backup.sh"

"test-backup.sh")
|#
