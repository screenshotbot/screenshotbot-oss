;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(uiop:define-package :screenshotbot/model/image
  (:use #:cl
        #:alexandria
        #:screenshotbot/model/view
        #:screenshotbot/magick
        #:screenshotbot/model/core
        #:screenshotbot/mask-rect-api
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
                #:find-image-by-id
                #:company
                #:verified-p ;; todo: remove, will cause conflict
                #:image-oid-cache)
  (:import-from #:bknr.datastore
                #:class-instances
                #:with-transaction
                #:store-object
                #:persistent-class)
  (:import-from #:screenshotbot/magick
                #:ping-image-dimensions
                #:magick)
  (:import-from #:bknr.indices
                #:unique-index)
  (:import-from #:bknr.indices
                #:hash-index)
  (:import-from #:util/hash-lock
                #:with-hash-lock-held
                #:hash-lock)
  (:import-from #:bknr.datastore
                #:store-object-id)
  (:import-from #:bknr.datastore
                #:blob-pathname)
  (:import-from #:auto-restart
                #:with-auto-restart)
  (:import-from #:screenshotbot/magick/magick-lw
                #:get-non-alpha-pixels
                #:with-image-comparison
                #:ping-image-metadata
                #:with-wand)
  (:import-from #:util/object-id
                #:make-oid
                #:oid
                #:oid-arr
                #:oid-p
                #:%make-oid
                #:oid-array)
  (:import-from #:util/digests
                #:md5-file)
  (:import-from #:util/store
                #:defindex
                #:def-store-local
                #:location-for-oid
                #:with-class-validation)
  (:import-from #:screenshotbot/cdn
                #:make-image-cdn-url)
  (:import-from #:screenshotbot/model/transient-object
                #:cannot-make-transient
                #:make-transient-clone
                #:with-transient-copy)
  (:import-from #:util/misc
                #:?.)
  (:import-from #:screenshotbot/installation
                #:installation
                #:installation-s3-store)
  (:import-from #:screenshotbot/s3/core
                #:s3-store-fetch-remote
                #:s3-store-update-remote)
  (:import-from #:util/copy-file
                #:copy-file-fast)
  (:import-from #:easy-macros
                #:def-easy-macro)
  (:import-from #:screenshotbot/events
                #:push-event)
  (:import-from #:bknr.indices
                #:unique-index)
  (:import-from #:bknr.datastore
                #:store-object)
  (:import-from #:util/threading
                #:with-extras)
  ;; classes
  (:export
   #:image
   #:image-blob
   #:mask-rect
   #:local-image)
  ;;methods
  (:export
   #:with-local-image
   #:image=
   #:image-public-url
   #:image-hash
   #:image-blob-get
   #:s3-key
   #:image-blob
   #:verified-p
   #:mask-rect-left
   #:rect-as-list
   #:mask-rect-width
   #:mask-rect-top
   #:mask-rect-height)
  (:export
   #:image-dimensions
   #:dimension
   #:dimension-height
   #:dimension-width
   #:image-format
   #:ping-image-dimensions
   #:find-image
   #:make-image
   #:image-filesystem-pathname
   #:update-image
   #:mask=
   #:image-metadata
   #:find-image-by-oid
   #:base-image-comparer
   #:dimension=
   #:image-size))
(in-package :screenshotbot/model/image)

(hex:declare-handler 'image-blob-get)

(defvar *image-creation-hooks*
  nil)

#+screenshotbot-oss
(with-class-validation
  (defclass image-blob (bknr.datastore:blob)
    ()
    (:metaclass persistent-class)))

(defindex +image-oid-index+ 'unique-index
  :test 'equalp
  :slot-name 'oid)

(defindex +image-hash-index+ 'hash-index
  :test #'equalp
  :slot-name 'hash)

(defparameter +image-state-filesystem+ 1
  "Image is saved on the local filesystem")

;; Some of these slots are limited to screenshotbot-oss. This is for
;; backward compatibility in the OSS version, where we don't have a
;; schema migration process.
(with-transient-copy (transient-image abstract-image
                      :extra-transient-slots (#-screenshotbot-oss blob))
  (defclass image (store-object)
    (#+screenshotbot-oss
     (link :initarg :link)
     (oid :accessor util/object-id:oid-struct-or-array
          :initarg :oid
          :reader image-oid
          :index +image-oid-index+
          :index-reader %find-image-by-oid)
     (hash :initarg :hash
           :reader image-hash ;; NOTE: Returns a vector!
           :index +image-hash-index+
           :index-reader images-for-original-hash)
     (state :initarg :state
            :initform nil
            :accessor %image-state
            :documentation "The state of the image. We use integers
           because they're cheaper to parse in bknr.datastore, and
           image objects are the largest number of objects in the
           store.")
     #+screenshotbot-oss
     (blob
      :initarg :blob
      :relaxed-object-reference t
      :accessor %image-blob ;; don't access directly!
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
     #+screenshotbot-oss
     (content-type :initarg :content-type
                   :reader image-content-type))
    (:metaclass persistent-class)
    (:default-initargs :oid (%make-oid))))

(defun imagep (image)
  (typep image 'abstract-image))

(defun check-imagep (image)
  (assert (imagep image))
  image)

(defmethod find-image-by-oid ((oid string))
  (find-image-by-oid
   ;; Convert to an array
   (mongoid:oid oid)))

(defmethod find-image-by-oid ((oid array))
  (find-image-by-oid
   ;; Convert to an OID object
   (make-oid :arr oid)))

(defmethod find-image-by-oid ((oid oid))
  (%find-image-by-oid oid))

(defmethod find-image-by-id ((company company) id)
  (let ((obj (find-image-by-oid id)))
    (typecase obj
      (local-image
       obj)
      (t
       (assert (eql company (company obj)))
       obj))))

#|

(loop for im in (bknr.datastore:class-instances 's3-blob)

       do (bknr.datastore:delete-object im))
|#

(defmethod make-transient-clone ((image image))
  (make-instance 'transient-image
                 :oid (oid-array image)
                 :hash (image-hash image)
                 :state (%image-state image)
                 :company (?. oid (company image))
                 :verified-p (verified-p image)))

(defmethod find-image ((company company) (hash string))
  (loop for image in (append
                      (images-for-original-hash hash)
                      (images-for-original-hash (ironclad:hex-string-to-byte-array hash)))
        if (and
            (eql (company image) company)
            (verified-p image))
          return image))

(defmethod find-image ((company company) (hash array))
  (find-image company
              (ironclad:byte-array-to-hex-string hash)))

(defmethod print-object ((self image) stream)
  (format stream "#<IMAGE ~a>" (store-object-id self)))

(with-transient-copy (transient-mask-rect abstract-mask-rect)
  (defclass mask-rect (store-object)
    ((top :initarg :top
          :accessor mask-rect-top)
     (left :initarg :left
           :accessor mask-rect-left)
     (height :initarg :height
             :accessor %mask-rect-height)
     (width :initarg :width
            :accessor %mask-rect-width))
    (:metaclass persistent-class)))

(defmethod mask-rect-top :around ((mask abstract-mask-rect))
  (let ((top (call-next-method)))
   (min
    top
    (+ top (%mask-rect-height mask)))))

(defmethod mask-rect-left :around ((mask abstract-mask-rect))
  (let ((left (call-next-method)))
    (min
     left
     (+ left (%mask-rect-width mask)))))

(defmethod mask-rect-height ((mask abstract-mask-rect))
  (abs (%mask-rect-height mask)))

(defmethod mask-rect-width ((mask abstract-mask-rect))
  (abs (%mask-rect-width mask)))

(defmethod mask= ((a abstract-mask-rect) (b abstract-mask-rect))
  (or
   (eql a b)
   (every #'identity
    (loop for fn in (list #'mask-rect-top
                          #'mask-rect-left
                          #'mask-rect-height
                          #'mask-rect-width)
          collect
          (eql
           (funcall fn a)
           (funcall fn b))))))

(defmethod make-transient-clone ((self mask-rect))
  (make-instance 'transient-mask-rect
                 :top (mask-rect-top self)
                 :left (mask-rect-left self)
                 :height (mask-rect-height self)
                 :width (mask-rect-width self)))

(defmethod rect-as-list ((rect mask-rect))
  (with-slots (top left height width) rect
    (list top left height width)))

(defmethod image-filesystem-pathname ((image abstract-image))
  "If the image is stored on the current file system, return the
  pathname to the image. If it's stored remotely, raise an error!"
  (cond
    ((eql +image-state-filesystem+ (%image-state image))
     (local-location-for-oid (oid-array image)))
    #+screenshotbot-oss
    ((%image-blob image)
     (bknr.datastore:blob-pathname (%image-blob image)))
    (t
     ;; the file most likely does not exist at this point, but this is
     ;; what you're asking for!
     (local-location-for-oid (oid-array image)))))

(defmethod image-not-uploaded-yet-p ((image image))
  (and
   (eql nil (%image-state image))))

(defmethod image-not-uploaded-yet-p ((image transient-image))
  nil)

(defclass local-image (image)
  ((url :initarg :url
        :accessor local-image-url))
  (:metaclass persistent-class)
  (:default-initargs :oid (%make-oid))
  (:documentation "An IMAGE, that's used only for testing purposes locally"))

(define-condition no-image-uploaded-yet (error)
  ((image :initarg :image))
  (:report (lambda (self stream)
             (format stream "No image uploaded for ~a" (slot-value self 'image)))))

(defmethod %with-local-image ((image abstract-image) fn)
  (cond
    ((image-not-uploaded-yet-p image)
     (error 'no-image-uploaded-yet :image image))
    (t
     (multiple-value-bind (file key) (image-filesystem-pathname image)
       (cond
         ((path:-e file)
          (funcall fn file))
         (t
          (s3-store-fetch-remote
           (installation-s3-store
            (installation))
           file
           key)
          (funcall fn file)))))))

;; todo: remove
(defmethod %with-local-image ((image image) fn)
  (call-next-method))

(defmethod %with-local-image ((image local-image) fn)
  ;; this could be bad if users have a way of creating local-images,
  ;; but they don't. It's always created in code for demo
  ;; purposes. (TODO: We should remove that logic, and use real images
  ;; instead).
  (funcall fn (asdf:system-relative-pathname
               :screenshotbot
               (format nil "static~a" (local-image-url image)))))

(defmacro with-local-image ((file screenshot) &body body)
  `(flet ((body (,file) ,@body))
     (%with-local-image ,screenshot #'body)))

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


(defun local-location-for-oid (oid)
  "Figure out the local location for the given OID"
  (location-for-oid
   #P"image-blobs/"
   oid))

(defun metadata-location-for-oid (oid)
  (location-for-oid
   #P "cl-store/image-metadata/"
   oid))

(defun make-image (&rest args &key hash blob pathname
                                oid ;; for test convenience
                                for-tests &allow-other-keys)
  (when blob
    (error "don't specify blob"))
  (unless (or hash pathname)
    (error "Must provide at least one of hash or pathname"))
  (let* ((args (alexandria:remove-from-plist args :pathname :for-tests))
         (oid (or oid (%make-oid)))
         (hash (cond
                 ((stringp hash)
                  (ironclad:hex-string-to-byte-array hash))
                 (t
                  hash))))
    (multiple-value-bind (image-file s3-key) (local-location-for-oid oid)
      ;; TODO: copy-overwriting-target could be a lot more efficient in
      ;; many cases.
      (when pathname
        (assert (path:-e pathname))
        (copy-file-fast pathname image-file)
        (unless for-tests ;; todo: refactor better
         (s3-store-update-remote
          (installation-s3-store (installation))
          image-file
          s3-key)))

      (apply #'make-instance 'image
               :oid oid
               :state (cond
                        (pathname
                         +image-state-filesystem+))
               :hash (cond
                       (hash hash)
                       (pathname
                        (md5-file image-file))
                       (t (error "must provide hash or pathname")))
               args))))

(defmethod update-image ((image image) &key pathname)
  (assert pathname)
  (with-transaction ()
    (setf (%image-state image)
          +image-state-filesystem+))
  (multiple-value-bind (dest key) (image-filesystem-pathname image)
    (uiop:copy-file pathname dest)
    (when key
      (s3-store-update-remote
       (installation-s3-store
        (installation))
       dest
       key))
    dest))

(with-class-validation
  (defclass content-equal-result (store-object)
    ((image-1 :initarg :image-1
              :index-type hash-index
              :index-initargs (:test #'equalp)
              :index-reader content-equal-results-for-image-1)
     (image-2 :initarg :image-2
              :reader image-2)
     (masks :initarg :masks
            :reader masks)
     (result :initarg :result
             :reader result))
    (:metaclass persistent-class)
    (:documentation "Comparing two images by content can be slow. This
  caches the result of such comparisons.")))

(defun clear-content-equal-results ()
  (mapc #'bknr.datastore:delete-object
          (bknr.datastore:store-objects-with-class 'content-equal-result)))

(defvar *content-equal-hash-lock* (make-instance 'hash-lock))

(define-condition slow-image-comparison ()
  ())

(defun images-equal-by-magick (img1 img2)
  "Use ImageMagick to check if the two images have identical contents"
  (log:info "Comparing images with magick: ~a ~a" img1 img2)
  (with-local-image (file1 img1)
    (with-local-image (file2 img2)
      (compare-image-files (magick) file1 file2))))

(defun images-equal-by-content-p (img1 img2 &key masks)
  (push-event :images-equal-by-content
              :img1 (oid img1))
  (with-hash-lock-held (img1 *content-equal-hash-lock*)
    (let ((existing-results (content-equal-results-for-image-1 (oid img1 :stringp nil))))
      (log:info "existing results: ~S" existing-results)
      (loop for result in existing-results
            if (and (equalp
                     (oid img2 :stringp nil)
                     (image-2 result))
                    (equal masks (masks result)))
              do (return (result result))
            finally
               (return
                 (flet ((save-result (result)
                          (make-instance 'content-equal-result
                                          :image-1 (oid img1 :stringp nil)
                                          :image-2 (oid img2 :stringp nil)
                                          :masks masks
                                          :result result)
                          result))
                   (log:info "[slow-path] checking images ~s, ~s with masks ~s" img1 img2 masks)
                   (signal 'slow-image-comparison)
                   (save-result
                    (with-local-image (file1 img1)
                      (with-local-image (file2 img2)
                        (with-wand (wand1 :file file1)
                          (with-wand (wand2 :file file2)
                            (with-image-comparison (wand1 wand2
                                                    :in-place-p t
                                                    :result result)
                              (eql
                               0
                               (first (array-dimensions (get-non-alpha-pixels
                                                         result
                                                         :limit 1
                                                         :masks masks))))))))))))))))

(defclass base-image-comparer ()
  ())

(defmethod image= ((self base-image-comparer) img1 img2 masks)
  "Check if the two images have the same contents. Looks at both file
  hash and image contents"
  (assert (image-hash img1))
  (assert (image-hash img2))
  (or
   (equalp (image-hash img1)
           (image-hash img2))
   ;; if the hash's don't match, check the perceptual hash. This is
   ;; slow, so make sure we're only doing this if absolutely required
   (when (or masks
             ;; A clever hack for allowing us to migrate from PNG to
             ;; WEBP without the user ever noticing. As long as the
             ;; PNG and the WEBP images are both lossless, they should
             ;; result in identical images.
             #+nil
             (not (string= (image-format img1)
                           (image-format img2))))
     (images-equal-by-content-p img1 img2 :masks masks))))

(defmethod image-hash ((image local-image))
  ;; this is probably only used for tests... hopefully doesn't hit in
  ;; prod.
  (with-local-image (im image)
    (md5-file im)))

(defmethod image-public-url ((image abstract-image) &key size type)
  (let ((url
         (let ((args nil))
           (when size
             (setf args `(:size ,(string-downcase size))))
           (when type
             (setf args (list* :type (str:downcase type) args)))
           (apply #'make-url 'image-blob-get :oid (encrypt:encrypt-mongoid (oid-array image))
                    args))))
    (cond
      (type
       (make-image-cdn-url url))
      (t
       ;; the image endpoint needs to guess the type based on Accept:
       ;; headers. So we don't cache this for now.
       url))))

(defmethod image-public-url ((image image) &key size type)
  (call-next-method))

(defmethod image-local-url ((image image))
  (image-public-url image))

(defmethod image-public-url ((image local-image) &key size)
  (local-image-url image))

(defmethod can-view ((image image) user)
  (can-view (company image) user))

(defclass metadata ()
  ((image-format :initarg :image-format
                 :reader metadata-image-format)
   (dimensions :initarg :dimensions
               :reader metadata-image-dimensions)))

(defclass dimension ()
  ((height :initarg :height
           :reader dimension-height)
   (width :initarg :width
          :reader dimension-width)))

(defmethod dimension= (dim1 dim2)
  (and
   (eql (dimension-height dim1)
        (dimension-height dim2))
   (eql (dimension-width dim1)
        (dimension-width dim2))))

(def-store-local *metadata-cache* (make-hash-table))

(defmethod image-metadata ((image abstract-image))
  (with-extras (("image" image))
    (util:or-setf
     (gethash image *metadata-cache*)
     (with-local-image (file image)
       (destructuring-bind (width height type)
           (ping-image-metadata (magick) file)
         (assert (member type '("WEBP" "PNG" "JPEG")
                         :test #'string=))
         (make-instance 'metadata
                        :image-format (intern type "KEYWORD")
                        :dimensions
                        (make-instance 'dimension
                                       :height height
                                       :width width)))))))

(defmethod invalidate-image-metadata ((image abstract-image))
  (remhash image *metadata-cache*))

(defmethod image-dimensions (image)
  (metadata-image-dimensions (image-metadata image)))

(defun image-file-dimensions (file)
  (destructuring-bind (width height) (ping-image-dimensions (magick) file)
      (make-instance 'dimension
                      :width width
                      :height height)))

(defun image-format (image)
  "Get the image format of the file. This looks at the magic in the
file content to determine the file type, not the pathname's
type. Example output could be either :PNG or :WEBP. If we can't
recognized the file, we'll return nil."
  (metadata-image-format (image-metadata image)))

(defun convert-all-images-to-webp ()
  "Converts all the images to lossless webp, while maintaining the
  original hashes. This way, any jobs referencing the old hashes will
  still not send a new image, and we can recover a lot of disk
  space."
  (loop for image in (reverse (bknr.datastore:store-objects-with-class 'image))
        for i from 0
        do
           (log:info "Converting image: ~a / ~a" i image)
           (restart-case
               (convert-image-to-webp image)
             (ignore-image ()
               (values)))))

(with-auto-restart ()
  (defun convert-image-to-webp (image)
   (let ((+limit+ 16383))
     (when (and
            (uiop:file-exists-p (image-filesystem-pathname image))
            (eql :png (image-format image))
            (let ((dim (image-dimensions image)))
              (and (< (dimension-height dim) +limit+)
                   (< (dimension-width dim) +limit+))))
       (flet ((%rename-file (from to)
                (log:info "Renaming file from ~a to ~a" from to)
                (rename-file from to)))
         (let ((path (make-pathname :type :unspecific
                                    :defaults (image-filesystem-pathname image))))
           (log:info "will switch image: ~a" path)
           (let ((tmp (make-pathname :type "webp"
                                     :defaults path))
                 (png (make-pathname :type "png"
                                     :defaults path)))
             (sleep 1)
             (invalidate-image-metadata image)
             (convert-to-lossless-webp
              (magick)
              path tmp)
             (invalidate-image-metadata image)
             (uiop:rename-file-overwriting-target tmp path)
             (assert (uiop:file-exists-p path)))))))))

(defun ensure-images-have-hash ()
  "Used as a migration to fix an issue with images having no hash"
  (let ((images (reverse
                 (loop for image in (bknr.datastore:store-objects-with-class 'image)
                       unless (image-hash image)
                         collect image))))
    (loop for image in images
          do
             (log:info "looking at: ~a" image)
             (with-local-image (file image)
               (let ((hash (md5-file file)))
                 (log:info "Got hash: ~a" hash)
                 (with-transaction ()
                   (setf (slot-value image 'hash) hash))
                 hash)))))

(defun delete-image-blob (oid))

(defun img-tmp-dir ()
  (ensure-directories-exist
   (path:catdir
    (bknr.datastore::store-directory
     bknr.datastore:*store*)
    "screenshotbot-tmp/")))

(defmacro with-tmp-image-file (args &body body)
  "Just like uiop:with-temporary-file, but uses a directory which is
 suitable for use with make-image. i.e. it will be on the same
 filesystem so we can hardlink instead of copying the image over if
 needed."
  `(uiop:with-temporary-file (:directory (img-tmp-dir) ,@args)
     ,@body))

(defun soft-expiration-time (image &key (months 3))
  (let* ((oid (cond
                ((oid-p image)
                 image)
                (t
                 (oid image :stringp nil))))
         (delta (- (mod (aref (oid-arr oid) 11) 64) 32))
         (oid-time (local-time:unix-to-timestamp
                    (mongoid:get-timestamp (oid-arr oid))))
         (midpoint (local-time:timestamp+
                     oid-time (* months 30) :day)))
    (local-time:timestamp+ midpoint delta :day)))


(defun all-soft-expired-images (&key (months 3))
  (let ((now (local-time:now)))
    (loop for im in (class-instances 'image)
          if (local-time:timestamp<
              (soft-expiration-time im :months months)
              now)
            collect im)))

(defvar *image-size-cache* (make-hash-table))

(defmethod image-size (image)
  (util:or-setf
   (gethash image *image-size-cache*)
   (handler-case
       (with-local-image (file image)
         (trivial-file-size:file-size-in-octets file))
     (no-image-uploaded-yet ()
       0)
     (zs3::xml-binding-error ()
       0))))

;;(length (all-soft-expired-images))
