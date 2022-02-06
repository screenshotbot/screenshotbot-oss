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
  (:import-from #:screenshotbot/magick
                #:run-magick)
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
   #:random-unequal-pixel
   #:draw-rects-in-place
   #:draw-masks-in-place))
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

(defclass fake-mask-rect ()
  ((top :initarg :top
        :accessor mask-rect-top)
   (left :initarg :left
         :accessor mask-rect-left)
   (height :initarg :height
           :accessor mask-rect-height)
   (width :initarg :width
          :accessor mask-rect-width))
  (:documentation "MASK-RECT for testing"))

(defmethod rect-as-list ((rect mask-rect))
  (with-slots (top left height width) rect
    (list top left height width)))

(defmethod s3-key ((image image))
  (check-type (image-blob image)
              s3-blob)
  (%s3-key (image-blob image)))

(defmethod %with-local-image ((image image) fn)
  (cond
    ((typep
      (image-blob image)
      'image-blob)
     (funcall fn (bknr.datastore:blob-pathname (image-blob image))))
    (t
     (uiop:with-temporary-file (:pathname p :stream s :direction :output :type "png"
                                :element-type 'flexi-streams:octet)
       (with-open-stream (remote (open-image-stream image))
         (uiop:copy-stream-to-stream remote s :element-type 'flexi-streams:octet))
       (finish-output s)
       (funcall fn p)))))

(defmacro with-local-image ((file screenshot) &body body)
  `(flet ((body (,file) ,@body))
     (%with-local-image ,screenshot #'body)))

(defun %draw-mask-rect-commands (masks &key color)
  "Imagemagick commands to draw rectangles for the given masks"
  `("-fill" ,color
    "-stroke" ,color
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
                                    (mask-rect-height mask)))))))

(defun draw-masks-in-place (image-file masks &key color)
  (when masks
    (uiop:with-temporary-file (:pathname tmp
                               :directory (cl-fad:pathname-directory-pathname image-file))
      (run-magick `("convert"
                          ,(namestring image-file)
                          ,@(%draw-mask-rect-commands masks :color color)
                          ,(namestring tmp)))
      (uiop:rename-file-overwriting-target
       tmp image-file))))

(defun %perceptual-hash (img masks)
  (log:info "Computing perceptual hash: ~s, ~s" img masks)
  (uiop:with-temporary-file (:pathname unused-output :type "tiff")
    (with-local-image (file img)
      (log:debug "Going to run convert")
      (str:trim
       ;; I can't use RUN-MAGICK for this because it creates a
       ;; different perpetual hash (the tests will fail). I think we
       ;; should modify this so that we don't need to use ImageMagick
       ;; to generate a hash of the image contents. IIRC, using the
       ;; perpetual hash is actually *incorrect* if we care about
       ;; determinism.
       (multiple-value-bind (out err ret)
           (uiop:run-program `("convert"
                               ,(namestring file)
                               ,@(%draw-mask-rect-commands masks
                                                           :color "black")
                               "-format" "%#"
                               "+write" "info:"
                               ,(namestring unused-output))
                             :output 'string
                             :error-output 'string
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

(defun call-with-opticl-image (img body)
  ;; todo: can we clean up the image immediately? In theory, if we use
  ;; static vectors and call pngload:wit-png-in-static-vector, this
  ;; will free the memory immediately, but only on SBCL and CCL.
  (with-local-image (file img)
    (let ((png (pngload:load-file file)))
     (funcall body (pngload:data png)))))

(defmacro with-opticl-image ((output img) &body body)
  `(let ((body (lambda (,output) ,@body)))
     (call-with-opticl-image ,img body)))

(defun find-unequal-pixels (img1 img2)
  (let ((res (make-array 0 :adjustable t
                           :fill-pointer t)))
    (map-unequal-pixels img1 img2
                        (lambda (i j)
                          (vector-push-extend (cons i j)
                                              res)))
    res))

(defun random-unequal-pixel (img1 img2 &key masks)
  (declare (optimize (speed 3) (safety 0)))
  (flet ((%map-unequal-pixels (fn)
           (map-unequal-pixels
            img1 img2 fn :masks masks)))
    (let ((num-bad 0))
      (declare (type fixnum num-bad))
      (%map-unequal-pixels
       (lambda (i j)
         (declare (ignore i j)
                  (optimize (speed 3) (safety 0) (debug 0)))
         (incf num-bad)))
      (let ((ctr (random num-bad)))
        (declare (type fixnum ctr))
        (%map-unequal-pixels
         (lambda (i j)
           (declare (fixnum i j))
           (when (= ctr 0)
             (return-from random-unequal-pixel
               (cons i j)))
           (decf ctr))))))
  (error "Should not get here, probably our CTR is off by one"))

(defun px-in-mask-p (i j mask)
  (declare (optimize (speed 3) (safety 0))
           (type fixnum i j))
  (and
   (<= (mask-rect-top mask)
       i
       (+ (mask-rect-top mask) (mask-rect-height mask) -1))
   (<= (mask-rect-left mask)
       j
       (+ (mask-rect-left mask) (mask-rect-width mask) -1))))


(defclass image-stream ()
  ((width :initarg :width
          :reader width)
   (height :initarg :height
           :reader height)
   (pos-x :initform 0
          :accessor pos-x)
   (pos-y :initform 0
          :accessor pos-y)))

(defclass image-stream-expanded-canvas (image-stream)
  ((delegate :initarg :delegate
             :reader delegate)))

(defmethod has-more-pixels-p ((self image-stream))
  (< (pos-y self)
     (height self)))

(defmethod next-pos ((self image-stream))
  (unless (has-more-pixels-p self)
    (error "Out of bounds for image"))
  (let ((old-x (pos-x self))
        (old-y (pos-y self)))
    (incf (pos-x self))
    (when (>= (pos-x self) (width self))
      (setf (pos-x self) 0)
      (incf (pos-y self)))
    (values old-y old-x)))

(defclass image-array-stream (image-stream)
  ((arr :initarg :arr
        :reader arr)
   (buffer :initarg :buffer
           :reader buffer)))

(defclass image-magick-stream (image-stream)
  ((%stream :accessor %stream)
   (file :initarg :file)
   (buffer :initform (make-array 4 :element-type '(unsigned-byte 8))
           :reader buffer)))

(defmethod initialize-instance :around ((self image-array-stream) &key arr)
  (let ((dims (array-dimensions arr)))
    (call-next-method
     self
     :arr arr
     :width (cadr dims)
     :height (car dims)
     :buffer (make-array (caddr dims)))))

(defmethod read-next-pixel ((image image-array-stream))
  (multiple-value-bind (pos-y pos-x) (next-pos image)
    (let ((buffer (buffer image)))
     (dotimes (j (length buffer))
       (setf (aref buffer j)
             (aref (arr image) pos-y pos-x j)))
      buffer)))

(defmethod read-next-pixel ((image image-stream-expanded-canvas))
  (multiple-value-bind (pos-y pos-x) (next-pos image)
    (let ((delegate (delegate image)))
     (cond
       ((or
         (>= pos-x (width delegate))
         (>= pos-y (height delegate)))
        ;; bad pixel!
        (dotimes (j (length (buffer delegate)))
          (setf (aref (buffer delegate) j) 0))
        (buffer delegate))
       (t
        (read-next-pixel delegate))))))

(defmethod initialize-instance :after ((self image-magick-stream) &key file &allow-other-keys)
  (setf (%stream self)
        (run-magick (list
                     "stream" "-map" "rgba"
                     "-storage-type" "char"
                     file
                     "-")
                    :async t)))

(defmethod cleanup-image-stream ((self image-magick-stream))
  (close (%stream self)))

(defmethod read-next-pixel ((self image-magick-stream))
  ;; we don't need the actual return value here
  (next-pos self)
  (read-sequence (buffer self) (%stream self))
  (buffer self))

(defun map-unequal-pixels-stream (stream1 stream2 fn &key masks)
  "Map unequal pixels assuming both streams refer to images with the same dimensions"
  (loop while (has-more-pixels-p stream1)
        for i = (pos-y stream1)
        for j = (pos-x stream1)
        for pix1 = (read-next-pixel stream1)
        for pix2 = (read-next-pixel stream2)
        if (not (equalp pix1 pix2))
          do
             ;; inefficient way to check if pixel is masked
             (loop for mask in masks
                   if (px-in-mask-p i j mask)
                     do (return nil)
                   finally
                      (funcall fn i j))))

(defun map-unequal-pixels-arr (arr1 arr2 fn &key masks)
  (let* ((dim1 (array-dimensions arr1))
         (dim2 (array-dimensions arr2))
         (height (max (car dim1) (car dim2)))
         (width (max (cadr dim1) (cadr dim2))))

    (flet ((make-expanded (x)
             (make-instance 'image-stream-expanded-canvas
                             :delegate x
                             :width width
                             :height height)))
     (map-unequal-pixels-stream
      (make-expanded
       (make-instance 'image-array-stream
                       :arr arr1))
      (make-expanded
       (make-instance 'image-array-stream
                       :arr arr2))
      fn
      :masks masks))))

(defun map-unequal-pixels (img1 img2 fn &key masks)
  (restart-case
      (with-local-image (file1 img1)
        (with-local-image (file2 img2)
          (flet ((make-image-stream (file)
                   (let ((png (pngload:load-file  file1 :decode nil)))
                     (make-instance 'image-magick-stream
                                     :file file
                                     :width (pngload:width png)
                                     :height (pngload:height png)))))
            (let ((stream1 (make-image-stream file1))
                  (stream2 (make-image-stream file2)))
              (unwind-protect
                   (map-unequal-pixels-stream
                    stream1
                    stream2
                    fn :masks masks)
                (cleanup-image-stream stream1)
                (cleanup-image-stream stream2))))))
    (retry-map-unequal-pixels ()
      (map-unequal-pixels img1 img2 fn :masks masks))))


(defun images-equal-by-content-p (img1 img2 &key (slow nil)
                                              masks)
  (log:info :image "checking images by content: ~s, ~s" img1 img2)
  (cond
    (slow
     (cond
       ((not (equalp (array-dimensions arr1) (array-dimensions arr2)))
        (log:info :image "Array dimension don't match: ~s, ~s" arr1 arr2)
        nil)
       (t
        (block check
          (let ((resp t))
            (map-unequal-pixels img1 img2
                                (lambda (i j)
                                  (declare (optimize (speed 3)
                                                     (safety 0)))
                                  (declare (ignore i j))
                                  (Setf resp nil)))
            resp)))))
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


(defmethod image-public-url ((image image) &key size)
  (cond
    ((typep (image-blob image) 'image-blob)
     (let ((args nil))
       (when size
         (setf args `(:size ,(string-downcase size))))
       (apply #'make-url 'image-blob-get :oid (oid image)
                args)))
    (t
     (format nil "https://screenshotbot.s3.amazonaws.com/~a"
             (s3-key image)))))

(defmethod image-local-url ((image image))
  (image-public-url image))

(defmethod image-public-url ((image local-image) &key size)
  (hex:make-full-url
   hunchentoot:*request*
   (local-image-url image)))

(defmethod can-view ((image image) user)
  (is-user-id-same image user))
