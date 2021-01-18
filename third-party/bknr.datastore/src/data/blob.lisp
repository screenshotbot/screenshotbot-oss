(in-package :bknr.datastore)

(cl-interpol:enable-interpol-syntax)

;;; blob

(defclass blob (store-object)
  ((type :initarg :type
         :reader blob-type
         :reader blob-mime-type
         :index-type hash-index
         :index-initargs (:test #'equal)
         :index-reader blobs-with-type)
   (timestamp :initarg :timestamp :reader blob-timestamp
              :initform (get-universal-time)))
  (:metaclass persistent-class))

#+nil
(define-persistent-class blob ()
  ((type :read)
   (timestamp :read))
  (:default-initargs :timestamp (get-universal-time)))

(defmethod print-object ((object blob) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "ID: ~D, TYPE: ~A"
            (store-object-id object)
            (if (slot-boundp object 'type) (blob-type object) "<not yet known>"))))

(defclass blob-subsystem ()
  ((n-blobs-per-directory
    :initform nil
    :initarg :n-blobs-per-directory
    :accessor n-blobs-per-directory
    :documentation "The number of blobs to store in each subdirectory of
blob-root.  If this is NIL, do not create subdirectories.  This parameter
must be specified when the when the store is created and is stored in the
datastore root to ensure that the correct value is used later.")))

(defun store-blob-root-pathname (&optional (store *store*))
  (merge-pathnames #p"blob-root/" (store-directory store)))

(defun store-blob-root-tempdir (&optional (store *store*))
  (merge-pathnames #p"temp/" (store-blob-root-pathname store)))

(defmethod initialize-subsystem ((subsystem blob-subsystem) store store-existed-p)
  (let* ((store-dir (store-current-directory store))
         (nblobs-pathname
          (make-pathname :name "n-blobs-per-directory" :defaults store-dir)))
    (if store-existed-p
        (if (probe-file nblobs-pathname)
            (unless (eql (n-blobs-per-directory subsystem)
                         (with-open-file (s nblobs-pathname)
                           (read s)))
              (error "BLOB configuration file ~A disagrees with user configuration"
                     nblobs-pathname))
            (progn
              (warn "Could not find stored number of blobs per directory, writing current value: ~S"
                    (n-blobs-per-directory subsystem))
              (with-open-file (s nblobs-pathname :direction :output)
                (write (n-blobs-per-directory subsystem) :stream s))))
        (with-open-file (s nblobs-pathname :direction :output)
          (write (n-blobs-per-directory subsystem) :stream s)))))

(defun blob-subsystem ()
  (or (find-if (lambda (subsystem)
                 (typep subsystem 'blob-subsystem))
               (store-subsystems *store*))
      (error "store ~A does not have a BLOB subsysten" *store*)))

(defmethod initialize-instance :before ((blob blob) &rest args)
  (declare (ignore args))
  (unless (blob-subsystem)
    (error "Can't create a BLOB in a datastore without BLOB subsystem.")))

(defmethod blob-relative-pathname (id)
  (let ((n-files (n-blobs-per-directory (blob-subsystem))))
    (if n-files
        (make-pathname
         :directory (list :relative (write-to-string (truncate id n-files)))
         :name (write-to-string id))
        (make-pathname :name (write-to-string id)))))

(defgeneric blob-pathname (blob-or-blob-id))

(defmethod blob-pathname ((id integer))
  (ensure-directories-exist (merge-pathnames (blob-relative-pathname id)
                                             (store-blob-root-pathname *store*)) :verbose t))

(defmethod blob-pathname ((blob blob))
  (blob-pathname (store-object-id blob)))

(defmacro with-open-blob ((s blob &rest args) &rest body)
  `(with-open-file (,s (blob-pathname ,blob) ,@args)
     ,@body))

(defgeneric blob-size (blob))

(defmethod blob-size ((blob blob))
  (with-open-blob (s blob)
    (file-length s)))

(defgeneric blob-to-stream (blob s))

(defmethod blob-to-stream ((blob blob) out)
  (with-open-blob (in blob :direction :input
                      :element-type '(unsigned-byte 8))
    (copy-stream in out)))

(defgeneric blob-to-file (blob pathname))

(defmethod blob-to-file ((blob blob) pathname)
  (with-open-file (out pathname :direction :output :element-type '(unsigned-byte 8))
    (blob-to-stream blob out)))

(defgeneric blob-from-stream (blob stream))

(defmethod blob-from-stream ((blob blob) in)
  (with-open-blob (out blob :direction :output
                       :element-type '(unsigned-byte 8)
                       :if-exists :overwrite
                       :if-does-not-exist :create)
    (copy-stream in out)))

(defgeneric blob-from-string (blob string))

(defmethod blob-from-string ((blob blob) string)
  (with-open-blob (out blob :direction :output
                       :if-exists :overwrite
                       :if-does-not-exist :create)
    (write-string string out)))

(defgeneric blob-from-array (blob array))

(defmethod blob-from-array ((blob blob) in)
  (with-open-blob (out blob :direction :output
                       :element-type '(unsigned-byte 8)
                       :if-exists :overwrite
                       :if-does-not-exist :create)
    (write-sequence in out)))

(defgeneric blob-from-file (blob pathname))

(defmethod blob-from-file ((blob blob) pathname)
  (with-open-file (in pathname :direction :input :element-type '(unsigned-byte 8))
    (blob-from-stream blob in)))

(defun make-blob-from-file (pathname &optional (class 'blob) &rest initargs)
  (unless (getf initargs :type)
    (setf (getf initargs :type)
          (pathname-type pathname)))
  (let ((blob (apply #'make-instance class initargs)))
    (blob-from-file blob pathname)
    blob))

(defmethod rename-file-to-blob ((blob blob) pathname)
  (move-file pathname (blob-pathname blob)))

(defmethod restore-subsystem ((store store) (subsystem blob-subsystem) &key until)
  (declare (ignore until))
  ;; the blob subsystem does not do anything upon restore
  )

(defmethod snapshot-subsystem ((store store) (subsystem blob-subsystem))
  (let* ((store-dir (ensure-store-current-directory store))
         (nblobs-pathname
          (make-pathname :name "n-blobs-per-directory" :defaults store-dir)))
    (with-open-file (s nblobs-pathname :direction :output)
      (write (n-blobs-per-directory subsystem) :stream s))))

(defun delete-orphaned-blob-files (&optional (cold-run t))
  (dolist (blob-pathname (directory (merge-pathnames (make-pathname :name :wild :directory '(:relative :wild-inferiors))
                                                     (store-blob-root-pathname))))
    (handler-case
        (when (pathname-name blob-pathname)
          (let* ((object-id (parse-integer (pathname-name blob-pathname)))
                 (object (find-store-object object-id)))
            (labels ((delete-orphan (pathname)
                       (handler-case
                           (if cold-run
                               (format t "cold run, not deleting ~A~%" pathname)
                               (delete-file pathname))
                         (error (e)
                           (warn "can't delete file ~A: ~A" pathname e)))))
              (cond
                ((null object)
                 (format t "; file ~A does not have a corresponding blob object - deleted~%" blob-pathname)
                 (delete-orphan blob-pathname))
                ((not (subtypep (type-of object) 'blob))
                 (format t "; file ~A has an object id of an object which does not have a subtype of blob (~A) - deleted~%"
                         blob-pathname (type-of object))
                 (delete-orphan blob-pathname))))))
      (error (e)
        (error "~A checking blob pathname ~A" e blob-pathname)))))
