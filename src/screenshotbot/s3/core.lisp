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
  (:import-from #:screenshotbot/async
                #:with-screenshotbot-kernel)
  (:import-from #:util/misc
                #:safe-ensure-directories-exist)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:null-store
   #:s3-store-update-remote))
(in-package :screenshotbot/s3/core)

(defclass base-store ()
  ())

(defclass s3-store (base-store)
  ((endpoint :initarg :endpoint
             :reader endpoint)
   (bucket :initarg :bucket
           :reader bucket)
   (access-key :initarg :access-key
               :reader access-key)
   (secret-access-key :initarg :secret-access-key
                      :reader secret-key))
  (:documentation "An S3 copy of the object-store"))

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

(defmethod s3-store-update-remote ((store s3-store) file key)
  (with-screenshotbot-kernel ()
    (lparallel:speculate
      (upload-file store file key))))

(defmethod s3-store-update-remote ((store null-s3-store) file key)
  nil)

(defmethod s3-store-fetch-remote :before ((store base-store) file key)
  (safe-ensure-directories-exist file))

(defmethod s3-store-fetch-remote ((store null-s3-store) file key)
  (error "Recovering images not supported: Can't fetch remote file from null s3 store"))

(defmethod s3-store-fetch-remote ((store s3-store) file key)
  (with-store (store)
    (zs3:get-file (bucket store) key file)))

#|
(upload-file *store* "/home/arnold/builds/web/backup.sh"

"test-backup.sh")
|#
