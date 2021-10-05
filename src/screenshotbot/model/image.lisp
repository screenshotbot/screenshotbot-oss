;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(uiop:define-package :screenshotbot/model/image
  (:use #:cl
        #:alexandria
        #:screenshotbot/model/view
        #:screenshotbot/model/core
        #:screenshotbot/screenshot-api)
  (:import-from #:util
                #:make-url
                #:oid-bytes
                #:oid
                #:object-with-oid)
  (:import-from #:screenshotbot/server
                #:document-root
                #:*root*)
  (:import-from #:screenshotbot/screenshot-api
                #:local-image)
  (:import-from #:screenshotbot/model/company
                #:company
                #:verified-p ;; todo: remove, will cause conflict
                #:image-oid-cache
                #:image-cache)
  (:import-from #:bknr.datastore
                #:with-transaction
                #:store-object
                #:persistent-class)
  ;; classes
  (:export
   #:image
   #:image-blob
   #:mask-rect
   #:local-image)
  ;;methods
  (:export
   #:with-local-image
   #:open-image-stream
   #:image=
   #:image-public-url
   #:image-hash
   #:image-blob-get
   #:s3-key
   #:s3-blob
   #:image-blob
   #:verified-p
   #:mask-rect-left
   #:rect-as-list
   #:mask-rect-width
   #:mask-rect-top
   #:mask-rect-height)
  (:export
   #:find-unequal-pixels
   #:random-unequal-pixel))
(in-package :screenshotbot/model/image)

(hex:declare-handler 'image-blob-get)

(defclass image-blob (bknr.datastore:blob)
  ()
  (:metaclass persistent-class))

(defclass s3-blob (store-object)
  ((s3-key :accessor %s3-key
           :initarg :s3-key))
  (:metaclass persistent-class))

(defmethod initialize-instance :around ((s3-blob s3-blob) &rest args)
  (let ((key (generate-api-secret)))
    (apply #'call-next-method s3-blob
             :s3-key key
            args)))

(defclass image (object-with-oid)
  ((link :initarg :link)
   (hash :initarg :hash
         :reader image-hash)
   (blob
    :initarg :blob
    :accessor image-blob
    :initform nil)
   (company
    :initarg :company
    :accessor company
    :initform nil)
   (verified-p
    :accessor verified-p
    :initform nil
    :initarg :verified-p
    :documentation "If we have verified that this image was uploaded")
   (content-type :initarg :content-type
                 :reader image-content-type))
  (:metaclass persistent-class))

(defun move-s3-key-to-blob ()
  "migration"
  (loop for i in (bknr.datastore:store-objects-with-class 'image)
        if (%s3-key i) do
          (with-transaction ()
            (setf (image-blob i) (make-instance 's3-blob
                                                 :s3-key (%s3-key i))))))
(defclass mask-rect (store-object)
  ((top :initarg :top
        :accessor mask-rect-top)
   (left :initarg :left
         :accessor mask-rect-left)
   (height :initarg :height
           :accessor mask-rect-height)
   (width :initarg :width
          :accessor mask-rect-width))
  (:metaclass persistent-class))

(defmethod rect-as-list ((rect mask-rect))
  (with-slots (top left height width) rect
    (list top left height width)))

(defmethod s3-key ((image image))
  (check-type (image-blob image)
              s3-blob)
  (%s3-key (image-blob image)))

(defmethod %with-local-image ((image image) fn)
  (uiop:with-temporary-file (:pathname p :stream s :direction :output :type "png"
                             :element-type 'flexi-streams:octet)
    (with-open-stream (remote (open-image-stream image))
      (fad:copy-stream remote s))
    (finish-output s)
    (funcall fn p)))

(defmacro with-local-image ((file screenshot) &body body)
  `(flet ((body (,file) ,@body))
     (%with-local-image ,screenshot #'body)))


(defun %perceptual-hash (img masks)
  (log:info "Computing perceptual hash: ~s, ~s" img masks)
  (uiop:with-temporary-file (:pathname unused-output :type "tiff")
    (with-local-image (file img)
      (log:debug "Going to run convert")
      (str:trim
       (multiple-value-bind (out err ret)
           (uiop:run-program `("convert"
                               ,(namestring file)
                               "-fill" "black"
                               "-stroke" "black"
                               ,@ (loop for mask in masks
                                        appending
                                        (list "-draw" (format nil "rectangle ~d,~d ~d,~d"
                                                              (mask-rect-left mask)
                                                              (mask-rect-top mask)
                                                              (+
                                                               (mask-rect-left mask)
                                                               (mask-rect-width mask))
                                                              (+
                                                               (mask-rect-top mask)
                                                               (mask-rect-height mask)))))
                               "-format" "%#"
                               "+write" "info:"
                               ,(namestring unused-output))
                             :output 'string
                             :error-outut 'string
                             :ignore-error-status t)
         (let ((out (str:trim out)))
           (unless (or
                    (eql ret 0)
                    (eql 64 (length out)))
             (error "`convert` failed. ~%stdout:~%~astderr:~%~A~%" out err))
           out)
)))))

(let ((cache (make-hash-table :test 'equal)))
  (defun perceptual-hash (img &key masks)
    (let ((cache-key (cons img (mapcar 'rect-as-list masks))))
     (or
      (gethash cache-key cache)
      (setf (gethash cache-key cache)
            (%perceptual-hash img masks))))))

(defun read-image-with-opticl (img)
  (with-open-stream (remote (open-image-stream img))
    (opticl:read-png-stream remote)))

(defun find-unequal-pixels (img1 img2)
  (let ((arr1 (read-image-with-opticl img1))
        (arr2 (read-image-with-opticl img2))
        (res (make-array 0 :adjustable t
                         :fill-pointer t)))
    (map-unequal-pixels arr1 arr2
                        (lambda (i j)
                          (vector-push-extend (cons i j)
                                              res)))
    res))

(defun random-unequal-pixel (img1 img2 &key masks)
  (declare (ignore masks)
           (optimize (speed 3) (safety 0)))
  (let ((arr1 (read-image-with-opticl img1))
        (arr2 (read-image-with-opticl img2)))
    (let ((num-bad 0))
      (declare (type fixnum num-bad))
      (map-unequal-pixels arr1 arr2
                          (lambda (i j)
                            (declare (ignore i j))
                            (incf num-bad))
                          :masks masks)
      (let ((ctr (random num-bad)))
        (declare (type fixnum ctr))
        (map-unequal-pixels arr1 arr2
                            (lambda (i j)
                              (declare (fixnum i j))
                              (when (= ctr 0)
                                (return-from random-unequal-pixel
                                  (cons i j)))
                              (decf ctr)))
        :masks masks))
    (error "Should not get here, probably our CTR is off by one")))

(defun map-unequal-pixels (arr1 arr2 fn &key masks)
  (let ((dim (array-dimensions arr1)))
    (dotimes (i (elt dim 0))
      (dotimes (j (elt dim 1))
        (block inner-loop
         (dotimes (k (elt dim 2))
           (unless (= (aref arr1 i j k)
                      (aref arr2 i j k))
             #+nil
             (log:info "Pixel (~a,~a, ~a) doesn't match, got ~s and ~s"
                       i j k
                       (aref arr1 i j k)
                       (aref arr2 i j k))
             (funcall fn i j)
             (return-from inner-loop))))))))


(defun images-equal-by-content-p (img1 img2 &key (slow nil)
                                              masks)
  (log:info :image "checking images by content: ~s, ~s" img1 img2)
  (cond
    (slow
     (let ((arr1 (read-image-with-opticl img1))
           (arr2 (read-image-with-opticl img2)))
       (cond
         ((not (equalp (array-dimensions arr1) (array-dimensions arr2)))
          (log:info :image "Array dimension don't match: ~s, ~s" arr1 arr2)
          nil)
         (t
          (block check
            (let ((resp t))
              (map-unequal-pixels arr1 arr2
                                  (lambda (i j)
                                    (declare (ignore i j))
                                    (Setf resp nil)))
              resp))))))
    (t
     (let ((hash1 (perceptual-hash img1 :masks masks))
           (hash2 (perceptual-hash img2 :masks masks)))
       (log:info :image "hashes: ~s, ~s" hash1 hash2)
       (when (equalp hash1 hash2)
         (log:info :image "Images have same perceptual hash: ~s, ~s" img1 img2)
         t)))))

(defun image= (img1 img2 masks)
  "Check if the two images have the same contents. Looks at both file
  hash and image contents"
  (log:info "checking images ~s, ~s" img1 img2)
  (or
   (string= (image-hash img1)
            (image-hash img2))
   ;; if the hash's don't match, check the perceptual hash. This is
   ;; slow, so make sure we're only doing this if absolutely required
   (when masks
    (images-equal-by-content-p img1 img2 :masks masks))))

(defun push-image-to-company-cache (image company)
  (push image (gethash (image-hash image) (image-cache company))))

(defmethod bknr.datastore:initialize-transient-instance :after ((image image))
  (when (company image)
    (push-image-to-company-cache image (company image))))

(defmethod (setf company) :after ((company company) (image image))
  (push-image-to-company-cache image company))

(defclass local-image (image)
  ((url :initarg :url
        :accessor local-image-url))
  (:metaclass persistent-class)
  (:documentation "An IMAGE, that's used only for testing purposes locally"))

(defmethod image-hash ((image local-image))
  ;; this is probably only used for tests... hopefully doesn't hit in
  ;; prod.
  (with-local-image (im image)
    (ironclad:byte-array-to-hex-string (md5:md5sum-file im))))

(defmethod external-file-name ((image image))
  (destructuring-bind (part ext) (str:split "/" (image-content-type image))
    (assert (equal "image" part))
    (format nil "~a.~a"
            (s3-key image)
            ext)))


(defmethod open-image-stream ((image image))
  (cond
    ((typep
      (image-blob image)
      'image-blob)
     (let ((pathname (bknr.datastore:blob-pathname (image-blob image))))
       (log:debug "Loading local image at: ~S" pathname)
       (open
        pathname
        :direction :input
        :element-type 'flexi-streams:octet)))
    (t
     (let ((url (image-public-url image)))
       (log:info "Fetching image: ~a" url)
       (dex:get url
                :force-binary t
                :want-stream t)))))

(defmethod open-image-stream ((image local-image))
  (open (path:catfile (document-root) (str:substring 1 nil (local-image-url image)))
        :direction :input
        :element-type 'flexi-streams:octet))


(defmethod image-public-url ((image image))
  (cond
    ((typep (image-blob image) 'image-blob)
     (make-url 'image-blob-get :oid (oid image)))
    (t
     (format nil "https://screenshotbot.s3.amazonaws.com/~a"
             (s3-key image)))))

(defmethod image-local-url ((image image))
  (image-public-url image))

(defmethod image-public-url ((image local-image))
  (hex:make-full-url
   hunchentoot:*request*
   (local-image-url image)))

(defmethod can-view ((image image) user)
  (is-user-id-same image user))
