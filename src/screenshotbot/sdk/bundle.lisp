;; Copyright 2018-Present Modern Interpreters Inc.
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(uiop:define-package :screenshotbot/sdk/bundle
  (:use #:cl
        #:alexandria)
  (:export
   #:image-stream
   #:close-bundle))
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

(defmethod close-bundle (bundle)
  (values))


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
              :accessor bundle-directory)
   (recursivep :initarg :recursivep
               :initform nil
               :reader recursivep)
   (file-types :initarg :file-types
               :initform (list "png")
               :reader file-types)))

(defmethod initialize-instance :after ((self image-directory) &key file-types &allow-other-keys)
  (assert (listp file-types))
  (loop for type in file-types
        if  (not (str:s-member '("png" "jpg" "jpeg" "heic" "jxl" "webp")
                               type :ignore-case t))
          do
             (error "Invalid image file type: '~a'" type)))

(defmethod override-image-pathname ((bundle image-directory)
                                    key pathname)
  pathname)

(defmethod list-images ((bundle image-directory))
  (labels ((parse-directory (dir prefix)
             (loop for im in (fad:list-directory dir)
                   if (and (recursivep bundle)
                           (fad:directory-pathname-p im))
                     append (parse-directory
                             im
                             (str:concat prefix (car (last (pathname-directory im))) "/"))
                   if (str:s-member (file-types bundle)
                                    (pathname-type im)
                                    :ignore-case t)
                     collect
                     (let ((key (str:concat prefix (pathname-name im))))
                       (let ((image (make-instance 'local-image
                                                   :name key
                                                   :pathname (override-image-pathname
                                                              bundle
                                                              key
                                                              im))))
                         image)))))
    (log:info "Parsing directory ~a" (bundle-directory bundle))
    (unwind-protect
         (parse-directory (bundle-directory bundle) "")
      (log:debug "Done parsing directory"))))


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
