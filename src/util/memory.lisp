(defpackage :util/memory
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:process-mem-usage
   #:arena-size
   #:histogram))
(in-package :util/memory)

(defun safe-type-of (x)
  (let ((type (type-of x)))
    (cond
      ((listp type)
       (Car type))
      (t
       type))))

(defun histogram (&key by-size)
  (let ((map (make-hash-table :test #'eql))
        (sizes (make-hash-table :test #'eql)))
    (hcl:sweep-all-objects
     (lambda (obj)
       (incf (gethash (safe-type-of obj) map 0))
       (incf (gethash (safe-type-of obj) sizes 0)
             (hcl:find-object-size obj))))
    (let ((list (loop for k being the hash-keys in map
                        using (hash-value v)
                      collect (cons k v))))
      (let ((ret (sort list #'>
                       :key (cond
                              (by-size
                               (lambda (x)
                                 (gethash (car x) sizes)))
                              (t #'cdr)))))
        (loop for (k . v) in ret
              for i below 100
              do (format t "~a ~s ~a~%" v k (gethash k sizes)))))))

(defun objects-of-type (type)
  (let ((ret))
    (hcl:sweep-all-objects
     (lambda (obj)
       (when (typep obj type)
         (push obj ret))))
    ret))

(defun weighted-random-sample (objects weight-fn)
  (let* ((total (loop for obj in objects
                      summing (funcall weight-fn obj)))
         (pt (random total)))
    (let ((curr 0))
      (dolist (obj objects)
        (incf curr (funcall weight-fn obj))
        (when (> curr pt)
          (return obj))))))

(defun process-mem-usage ()
  (let ((ret 0))
    #+linux
    (let ((pid (util/posix:getpid)))
      (with-open-file (stream (format nil "/proc/~d/smaps" pid))
        (loop for line = (read-line stream nil nil)
              while line
              for parts = (str:split ":" line)
              do
                 (when (string= "Pss" (first parts))
                   (incf ret (parse-integer (str:trim (second parts))  :junk-allowed t))))))
    ret))


;; (histogram)
;; (random-sample:random-sample (objects-of-type 'lw:simple-text-string) 100)
;; (weighted-random-sample (objects-of-type 'lw:simple-text-string) #'hcl:find-object-size)


(fli:define-foreign-function fmemopen
    ((buf :lisp-simple-1d-array)
     (size :size-t)
     (open-type (:reference-pass :ef-mb-string)))
  :result-type :pointer)

(fli:define-foreign-function fclose
    ((buf :pointer))
  :result-type :int)

(fli:define-foreign-function (%malloc-info "malloc_info")
    ((options :int)
     (string :pointer))
  :result-type :int)

;; mallinfo2 is the same as mallinfo but uses :size-t. However, it's
;; introduced in glibc 2.33, which is currently not what I'm using. In
;; the future we can change this.
(fli:define-c-struct mallinfo
    (arena :int
           :documentation "Non-mmaped space allocated (bytes)")
  (ordblks :int)
  (smblks :int)
  (hblks :int
         :documentation "Number of mmapped regions")
  (hblkhd :int
          :documentation "Space allocation in mmaped regions (bytes)")
  (usmblks :int)
  (fsmblks :int)
  (uordblks :int)
  (fordblks :int)
  (keepcost :int))

(fli:define-foreign-function mallinfo
    ()
  :result-type mallinfo)

(defun arena-size ()
  (fli:with-dynamic-foreign-objects ((mallinfo mallinfo))
    (mallinfo :result-pointer mallinfo)
    (values
     (fli:foreign-slot-value mallinfo 'arena)
     (fli:foreign-slot-value mallinfo 'hblkhd))))

(defun malloc-info ()
  (let* ((size 202400)
         (str (make-array (+ 100 size)
                          :element-type '(unsigned-byte 8)
                          :allocation :pinnable)))
    (let ((file (fmemopen str size "w")))
      (unwind-protect
           (let ((err (%malloc-info 0 file)))
             (unless (= err 0)
               (error "malloc-info failed ~a" err))
             (flex:octets-to-string str
                                    :end (position 0 str)))
        (fclose file)))))

;; (format t "~a" (malloc-info))
