;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/replay/run-builder
  (:use #:cl)
  (:nicknames :screenshotbot/pro/replay/run-builder)
  (:import-from #:screenshotbot/sdk/bundle
                #:local-image
                #:list-images)
  (:import-from #:screenshotbot/model/image
                #:image-blob
                #:make-image
                #:find-image)
  (:import-from #:bknr.datastore
                #:blob-pathname)
  (:import-from #:screenshotbot/sdk/sdk
                #:upload-image-directory)
  (:import-from #:util/object-id
                #:oid)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:recorder-run-builder
   #:all-screenshots
   #:record-screenshot))
(in-package :screenshotbot/replay/run-builder)

(defclass screenshot ()
  ((title :initarg :title
          :reader screenshot-title)
   (image :initarg :image
          :reader screenshot-image)))

(defclass all-screenshots ()
  ((screenshots :initform nil
                :accessor screenshots)
   (company :initarg :company
            :accessor company)))

(defmethod record-screenshot ((self all-screenshots)
                              &key title pathname &allow-other-keys)
  (let ((hash (md5:md5sum-file pathname)))
    (let ((image (or
                  (find-image (company self) hash)
                  (let ((blob (make-instance 'image-blob)))
                    (fad:copy-file pathname (blob-pathname blob))
                    (make-image :blob blob
                                :company (company self)
                                :hash hash
                                :verified-p t)))))
      (push (make-instance
             'screenshot
              :title title
              :image image)
            (screenshots self)))))


(defmethod list-all-screenshots ((self all-screenshots))
  (error "unimplemented"))


(defmethod list-images ((self all-screenshots))
  (error "unimplemented"))

(defmethod upload-image-directory ((self all-screenshots))
  (loop for im in (screenshots self)
        collect `((:id . ,(oid (screenshot-image im)))
                  (:name . ,(screenshot-title im)))))
