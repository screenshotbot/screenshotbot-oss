(defpackage :screenshotbot/model/image-comparison
  (:use #:cl)
  (:shadow #:find)
  (:import-from #:bknr.datastore
                #:store-object-id
                #:snapshot-subsystem-async
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
                #:calculate-difference-rmse
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
                #:close-subsystem)
  (:import-from #:bknr.indices
                #:object-destroyed-p)
  (:import-from #:bknr.datastore
                #:store-object)
  (:import-from #:util/store/store
                #:all-subsystem-objects)
  (:import-from #:util/store/store-migrations
                #:def-store-migration)
  (:import-from #:util/events
                #:with-tracing)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:image-comparison
   #:%image-comparisons-for-before
   #:image-comparison-after
   #:image-comparison-result
   #:identical-p
   #:image-comparison-difference-value))
(in-package :screenshotbot/model/image-comparison)

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
     (difference-value :initarg :difference-value
                       :initform nil
                       :accessor image-comparison-difference-value
                       :documentation "The difference as computed as a value, currently only RMSE metric. Will be a DOUBLE or NIL for older image-comparisons where this was not computed.")
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

(deftransaction tx-remove-image-comparison (before after)
  "Deletes the image-comparison object with BEFORE and AFTER."
  (setf *stored-cache*
        (fset:less *stored-cache* (make-instance 'transient-image-comparison
                                                 :before before
                                                 :after after))))

(defun remove-image-comparison (before after)
  (tx-remove-image-comparison before after))

(defun do-image-comparison (before-image
                            after-image
                            p)
  "Compares before-screenshot and after-screenshot, and saves the result image to P.

Returns two values. If the images are identical, we return T for the
primary value, else we return NIL.

Second second value will be the RMSE difference between the two
images. If the first value was T, then this will always be 0.0"
  (with-tracing (:image-comparison)
    (with-local-image (before-file before-image)
      (with-local-image (after-file after-image)
        (with-wand (before :file before-file)
          (with-wand (after :file after-file)
            (let ((rmse (calculate-difference-rmse before after)))
             (let ((same-p (compare-wands before after p
                                          :in-place-p t)))
               (values same-p rmse)))))))))

(defmethod find-image-comparison-on-images ((before image)
                                            (after image)
                                            &key only-cached-p)
  "Finds an existing image comparison for before and after, if it
  doesn't exist calls creator with a temporary file. The creator
  should create the image in the file provided. The creator should
  returns true if the images are completely identical, or nil
  otherwise

  If CACHE-ONLY is T, we won't create the image-comparison, and should
  respond quickly."
  
  (when (> (store-object-id before)
           (store-object-id after))
    (rotatef before after))

  (unless (eql (company before)
               (company after))
    (error "Trying to compare images from different companies"))

  (flet ((find ()
           (let ((res (find-image-comparison-from-cache :before before :after after)))
             (when (and res (not only-cached-p))
              (maybe-populate-difference-value res))
             res)))
    (or
     (find)
     (unless only-cached-p
       (with-tmp-image-file (:pathname p :type "webp" :prefix "comparison")
         (multiple-value-bind (identical-p
                               difference-value)
             (do-image-comparison before after p)
           (let* ((image (make-image :pathname p
                                     :company (company after))))
             (make-image-comparison
              :before before
              :after after
              :identical-p identical-p
              :difference-value difference-value
              :result image))))))))

(defun maybe-populate-difference-value (image-comparison)
  "As a migration strategy for T1547, if the difference-value is not
already present, let's recompute it. We expect this logic to not be
hit too much in production."
  (cond
    ((image-comparison-difference-value image-comparison)
     (values))
    (t
     (with-tmp-image-file (:pathname output-image :type "webp")
       (multiple-value-bind (identical-p difference-value)
           (do-image-comparison (image-comparison-before image-comparison)
             (image-comparison-after image-comparison)
             output-image)
         (declare (ignore identical-p))
         (setf (image-comparison-difference-value image-comparison)
               difference-value))))))


(defclass image-comparison-subsystem ()
  ())

(defconstant +snapshot-version+ 1)

(defun gc-comparisons ()
  (fset:do-set (var *stored-cache*)
    (flet ((destroyed? (obj)
             (or (not obj)
                 (and
                  (typep obj 'store-object)
                  (object-destroyed-p obj)))))
      (when (or (destroyed? (image-comparison-before var))
                (destroyed? (image-comparison-after var))
                (destroyed? (image-comparison-result var)))
        (setf *stored-cache*
              (fset:less *stored-cache* var))))))

(defun write-snapshot (pathname stored-cache)
  (log:info "Snapshotting image-comparison subsystem")
  (with-open-file (output pathname
                          :direction :output
                          :element-type '(unsigned-byte 8)
                          :if-does-not-exist :create
                          :if-exists :supersede)
    (encode +snapshot-version+ output)
    ;; write the number of objects too
    (encode (fset:size stored-cache) output)
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
      (write-imcs stored-cache))))

(defmethod snapshot-subsystem-async ((store bknr.datastore:store) (self image-comparison-subsystem))
  (gc-comparisons)
  (let ((pathname (store-subsystem-snapshot-pathname store self))
        (stored-cache *stored-cache*))
    (lambda ()
      (Write-snapshot pathname stored-cache))))

(defmethod restore-subsystem ((store bknr.datastore:store) (self image-comparison-subsystem) &key until)
  (declare (ignore until) (optimize (debug 3)))
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
                                for obj = (read-single)
                                ;; When we copy this snapshot to
                                ;; different server, some images may
                                ;; not be available.
                                #+lispworks #+lispworks
                                if (and
                                    (image-comparison-before obj)
                                    (image-comparison-after obj))
                                  collect obj)))
                (setf *stored-cache*
                      (fset:convert 'fset:set objs)))))))))
  (log:info "Loaded ~a image-comparison objects" (fset:size *stored-cache*)))

(defmethod all-subsystem-objects ((self image-comparison-subsystem))
  (fset:convert 'list *stored-cache*))

(defmethod close-subsystem ((store bknr.datastore:store) (self image-comparison-subsystem))
  (setf *stored-cache* (fset:empty-set)))

(defsubsystem image-comparison-subsystem :priority 20)

(def-store-migration ("Add company slot to previous results" :version 11)
  (fset:do-set (imc *stored-cache*)
    (alexandria:when-let ((after (image-comparison-after imc))
                          (result (image-comparison-result imc)))
      (unless (company result)
        (setf (company result) (company after))))))
