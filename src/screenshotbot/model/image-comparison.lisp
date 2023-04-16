(defpackage :screenshotbot/model/image-comparison
  (:use #:cl)
  (:shadow #:find)
  (:import-from #:bknr.datastore
                #:persistent-class)
  (:import-from #:screenshotbot/model/transient-object
                #:make-transient-clone
                #:with-transient-copy)
  (:import-from #:bknr.datastore
                #:store-object)
  (:import-from #:bknr.indices
                #:hash-index)
  (:import-from #:screenshotbot/model/image
                #:find-image-by-oid
                #:mask-rect-height
                #:mask-rect-width
                #:mask-rect-top
                #:mask-rect-left
                #:with-tmp-image-file
                #:mask=
                #:with-local-image
                #:make-image
                #:image)
  (:import-from #:auto-restart
                #:with-auto-restart)
  (:import-from #:bknr.datastore
                #:delete-object)
  (:import-from #:util/object-id
                #:oid
                #:oid-array)
  (:import-from #:util/store
                #:add-datastore-cleanup-hook
                #:object-store
                #:location-for-oid)
  (:import-from #:bknr.datastore
                #:class-instances)
  (:import-from #:bknr.datastore
                #:store-objects-with-class)
  (:import-from #:util/cron
                #:def-cron)
  (:import-from #:screenshotbot/events
                #:push-event)
  (:import-from #:screenshotbot/magick/magick-lw
                #:compare-wands
                #:with-image-comparison
                #:with-wand)
  (:import-from #:util/misc
                #:?.)
  (:import-from #:screenshotbot/model/company
                #:company)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:image-comparison
   #:%image-comparisons-for-before
   #:image-comparison-after
   #:image-comparison-result
   #:identical-p))
(in-package :screenshotbot/model/image-comparison)

(defvar *lock* (bt:make-lock "image-comparison"))

(defvar *db* nil)

(defun clean-db ()
  (when *db*
    (sqlite:disconnect *db*)
    (setf *db* nil)))

#+lispworks
(unless (hcl:delivered-image-p)
 (lw:define-action "Delivery Actions" "Clean image-comparison db"
   'clean-db))

(add-datastore-cleanup-hook 'clean-db)

(defun ensure-db ()
  (util:or-setf
   *db*
   (let ((db
          (sqlite:connect
           (ensure-directories-exist
            (path:catfile (object-store) "sqlite/image-comparisons.db")))))
     (prepare-schema db)
     db)
   :thread-safe t))

(defun prepare-schema (db)
  (sqlite:execute-non-query
   db
   ;; before, after, and result are OIDs to images represented as
   ;; strings.
   "create table if not exists comparisons
     (before text not null, after text not null,
      identical_p int,
      result text not null)")
  (sqlite:execute-non-query
   db
   "create unique index if not exists comparisons_image_lookup
    on comparisons (before, after)"))

(with-transient-copy (transient-image-comparison abstract-image-comparison)
  (defclass image-comparison (store-object)
    ((before :initarg :before
             :reader image-comparison-before
             :index-type hash-index
             :index-reader %image-comparisons-for-before
             :relaxed-object-reference t)
     (after :initarg :after
            :reader image-comparison-after
            :relaxed-object-reference t)
     (masks :initarg :masks
            :initform nil
            :documentation "DEPRECATED: do not use.")
     (identical-p :initform nil
                  :accessor identical-p
                  :initarg :identical-p
                  :documentation "A result inducating that the images differ only in exif data")
     (result :initarg :result
             :accessor image-comparison-result
             :documentation "The final image object"))
    (:metaclass persistent-class)))

(defmethod sqlite-write-comparison ((self abstract-image-comparison))
  (sqlite:execute-non-query
   (ensure-db)
   "insert or replace into comparisons
      (before, after, identical_p, result)
 values (?, ?, ?, ?)
"
   (oid (image-comparison-before self))
   (oid (image-comparison-after self))
   (if (identical-p self) 1 0)
   (oid (image-comparison-result self)))
  self)

(defun sqlite-read-comparison (before after)
  (multiple-value-bind (identical-as-num result-oid)
      (sqlite:execute-one-row-m-v
       (ensure-db)
         "select identical_p, result from comparisons where
    before = ? and after = ?"
         (oid before)
         (oid after))
    (when identical-as-num
      (assert (member identical-as-num '(0 1)))
      (make-instance 'transient-image-comparison
                     :before before
                     :after after
                     :identical-p (= 1 identical-as-num)
                     :result (find-image-by-oid result-oid)))))

(defun make-old-transient ()
  (values))

;; TODO: remove
(def-cron make-old-transient (:minute 45 :hour 22)
  (make-old-transient))


(defun do-image-comparison (before-image
                            after-image
                            p)
  "Compares before-screenshot and after-screenshot, and saves the result image to P.

If the images are identical, we return t, else we return NIL."
  (with-local-image (before-file before-image)
    (with-local-image (after-file after-image)
      (with-wand (before :file before-file)
        (with-wand (after :file after-file)
          (let ((same-p (compare-wands before after p
                                       :in-place-p t)))
            same-p))))))

(defmethod find-image-comparison-on-images ((before image)
                                            (after image))
  "Finds an existing image comparison for before and after, if it
  doesn't exist calls creator with a temporary file. The creator
  should create the image in the file provided. The creator should
  returns true if the images are completely identical, or nil
  otherwise"
  (flet ((find ()
           (sqlite-read-comparison before after)))
    (or
     (bt:with-lock-held (*lock*)
       (find))
     (with-tmp-image-file (:pathname p :type "webp" :prefix "comparison")
       (let ((identical-p (do-image-comparison
                            before
                            after
                            p)))
         (let* ((image (make-image :pathname p)))
           (bt:with-lock-held (*lock*)
             (or
              (find)
              (progn
                (log:info "making new image-comparison")
                (sqlite-write-comparison
                 (make-instance 'transient-image-comparison
                                :before before
                                :after after
                                :identical-p identical-p
                                :result image)))))))))))

(with-auto-restart ()
  (defmethod recreate-image-comparison ((self image-comparison))
    (log:info "recreating: ~a" (bknr.datastore:store-object-id self))
    (let* ((image (image-comparison-result self)))
      (check-type image image)
      (with-local-image (pathname image)
        (restart-case
            (progn
              (let ((before (image-comparison-before self))
                    (after (image-comparison-after self)))
                (delete-object self)
                (find-image-comparison-on-images
                 before after))
              (delete-object image)
              (log:info "Deleting ~a" pathname)
              (delete-file pathname))
          (ignore-this-comparison ()
            (values)))))))


(defmethod recreate-all-image-comparisons ()
  (loop for image-comparison in (reverse
                                 (bknr.datastore:store-objects-with-class 'image-comparison))
        do
        (recreate-image-comparison image-comparison)))
