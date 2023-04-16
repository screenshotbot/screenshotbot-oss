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
                #:defsubsystem
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
  (:import-from #:bknr.datastore
                #:deftransaction)
  (:import-from #:bknr.datastore
                #:store-subsystem-snapshot-pathname)
  (:import-from #:bknr.datastore
                #:encode-object)
  (:import-from #:bknr.datastore
                #:decode-object)
  (:import-from #:bknr.datastore
                #:encode)
  (:import-from #:bknr.datastore
                #:decode)
  (:import-from #:bknr.datastore
                #:restore-subsystem)
  (:import-from #:bknr.datastore
                #:snapshot-subsystem)
  (:import-from #:bknr.datastore
                #:close-subsystem)
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

(defmethod fset:compare ((a transient-image-comparison)
                         (b transient-image-comparison))
  (fset:compare-slots a b
                      #'image-comparison-before
                      #'image-comparison-after))

(defvar *stored-cache*
  (fset:empty-set)
  "A cache of empty image-comparison")

(deftransaction %make-image-comparison (args)
  (let ((imc (apply #'make-instance
                    'transient-image-comparison
                    args)))
    (setf *stored-cache*
          (fset:with *stored-cache* imc))
    imc))

(defun find-image-comparison-from-cache (&key before after)
  (multiple-value-bind (exists-p value)
      (fset:lookup
       *stored-cache*
       (make-instance 'transient-image-comparison
                      :before before
                      :after after))
    (declare (ignore exists-p))
    value))

(defun make-image-comparison (&rest args)
  (%make-image-comparison args))

(deftransaction remove-image-comparison (imc)
  (setf *stored-cache*
        (fset:less *stored-cache* imc)))

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

(defclass image-comparison-subsystem ()
  ())

(defconstant +snapshot-version+ 1)

(defmethod snapshot-subsystem ((store bknr.datastore:store) (self image-comparison-subsystem))
  (log:info "Snapshotting image-comparison subsystem")
  (with-open-file (output (store-subsystem-snapshot-pathname store self)
                          :direction :output
                          :element-type '(unsigned-byte 8)
                          :if-does-not-exist :create
                          :if-exists :supersede)
    (encode +snapshot-version+ output)
    ;; write the number of objects too
    (encode (fset:size *stored-cache*) output)
    (labels ((write-single (obj)
               (encode (image-comparison-before obj) output)
               (encode (image-comparison-after obj) output)
               (encode (image-comparison-result obj) output)
               (encode
                (if (identical-p obj) 1 0)
                output))
             (write-imcs (set)
               (cond
                 ((fset:empty? set)
                  nil)
                 (t
                  (let ((next (fset:arb set)))
                    (write-single next)
                    (write-imcs (fset:less set next)))))))
      (write-imcs *stored-cache*))))

(defmethod restore-subsystem ((store bknr.datastore:store) (self image-comparison-subsystem) &key until)
  (declare (ignore until))
  (log:info "Restoring image-comparison subsystem")
  (let ((snapshot-file (store-subsystem-snapshot-pathname store self)))
    (when (probe-file snapshot-file)
      (with-open-file (stream snapshot-file
                              :direction :input
                              :element-type '(unsigned-byte 8))
        (let ((version (decode stream)))
          (unless (<= version +snapshot-version+)
            (error "Unsupported version for image comparisons snapshot: ~a" version))
          (let ((size (decode stream)))
            (flet ((read-single ()
                     (make-instance 'transient-image-comparison
                                    :before (decode stream)
                                    :after (decode stream)
                                    :result (decode stream)
                                    :identical-p (ecase (decode stream)
                                                   (1 t)
                                                   (0 nil)))))
              (let ((objs (loop for i from 0 below size
                                collect (read-single))))
                (setf *stored-cache*
                      (fset:convert 'fset:set objs))))))))))

(defmethod close-subsystem ((store bknr.datastore:store) (self image-comparison-subsystem))
  (setf *stored-cache* (fset:empty-set)))

(defsubsystem image-comparison-subsystem :priority 20)
