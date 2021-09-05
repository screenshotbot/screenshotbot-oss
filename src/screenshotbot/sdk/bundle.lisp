;; Copyright 2018-Present Modern Interpreters Inc.
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(uiop:define-package :screenshotbot/sdk/bundle
  (:use #:cl
        #:alexandria)
  (:export
   #:image-stream))
(in-package :screenshotbot/sdk/bundle)


(defclass abstract-image ()
  ((cached-md5 :initform nil
              :accessor cached-md5)
   (lock :initform (bt:make-lock))
   (cv :initform (bt:make-condition-variable))))

(defclass local-image (abstract-image)
  ((name :initarg :name
         :accessor image-name)
   (pathname :initarg :pathname
             :accessor image-pathname)))

(defclass streamed-image (abstract-image)
  ((name :initarg :name
         :accessor image-name)
   (stream :initarg :stream
           :accessor image-stream)))

(defmethod close-image ((local-image local-image)))

(defmethod close-image ((image streamed-image))
  (close (image-stream image)))

(defmethod %md5-sum (image)
  (let ((stream (image-stream image)))
    (unwind-protect
        (ironclad:byte-array-to-hex-string (md5:md5sum-stream stream))
      (etypecase image
        (local-image
         (close stream))
        (t
         (file-position stream 0))))))

(defun md5-sum (image)
  (or (cached-md5 image)
      (setf (cached-md5 image) (%md5-sum image))))

(defclass image-directory ()
  ((directory :initarg :directory
              :accessor bundle-directory)))

(defmethod override-image-pathname ((bundle image-directory)
                                    key pathname)
  pathname)

(defmethod list-images ((bundle image-directory))
  (loop for im in (fad:list-directory (bundle-directory bundle))
        if (equal "png" (pathname-type im))
          collect
        (let ((key (pathname-name im)))
          (let ((image (make-instance 'local-image
                                      :name key
                                      :pathname (override-image-pathname
                                                 bundle
                                                 key
                                                 im))))
            image))))

(defmethod image-stream ((im local-image))
  (open (image-pathname im) :direction :input
                            :element-type 'flexi-streams:octet))

(defclass image-directory-with-diff-dir (image-directory)
  ((diff-dir :initarg :diff-dir)))

(defmethod override-image-pathname ((bundle image-directory-with-diff-dir)
                                    key
                                    pathname)
  (with-slots (diff-dir) bundle
   (let ((potential-file (path:catfile diff-dir
                                       (format nil "failed_~a.png" key))))
     (cond
       ((path:-e potential-file)
        potential-file)
       (t
        pathname)))))
