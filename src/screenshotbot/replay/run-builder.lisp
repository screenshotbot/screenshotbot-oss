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
                #:with-tmp-image-file
                #:image-blob
                #:make-image
                #:find-image)
  (:import-from #:bknr.datastore
                #:blob-pathname)
  (:import-from #:screenshotbot/sdk/sdk
                #:upload-image-directory)
  (:import-from #:util/object-id
                #:oid)
  (:local-nicknames (#:a #:alexandria)
                    (#:dto #:screenshotbot/api/model))
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
                              &key title md5 fetch)
  (let ((hash md5))
    (let ((image (or
                  (find-image (company self) hash)
                  (with-tmp-image-file (:pathname tmpfile)
                    (delete-file tmpfile)
                    (funcall fetch tmpfile)
                    (make-image :company (company self)
                                :pathname tmpfile
                                :hash hash
                                :verified-p t)))))
      (atomics:atomic-push
       (make-instance
        'screenshot
        :title title
        :image image)
       (slot-value self 'screenshots)))))


(defmethod list-all-screenshots ((self all-screenshots))
  (error "unimplemented"))


(defmethod list-images ((self all-screenshots))
  (error "unimplemented"))

(defmethod upload-image-directory (api-context (self all-screenshots))
  (declare (ignore api-context))
  (loop for im in (remove-duplicates (screenshots self) :test #'equal :key #'screenshot-title)
        collect
        (make-instance 'dto:screenshot
                       :image-id (oid (screenshot-image im))
                       :name (screenshot-title im))))
