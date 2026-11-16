(defpackage :util/memory
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:process-mem-usage
   #:arena-size
   #:histogram
   #:room-table))
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

;;; ROOM-TABLE

(defun parse-room-output (output)
  "Parses the output of (room :full) into a list of (GEN-NUM TOTAL
ALIST), where ALIST maps each allocation type name to the bytes
allocated, and returns the total allocated and total size as the
second and third values."
  (let ((gens)
        (total-allocated)
        (total-size))
    (dolist (line (str:lines output))
      (cond
        ((cl-ppcre:register-groups-bind ((#'parse-integer gen total))
             ("^ > Generation (\\d+): (\\d+) \\(" line)
           (push (list gen total nil) gens)))
        ((cl-ppcre:register-groups-bind (type (#'parse-integer size))
             ("^\\s+([A-Za-z-]+)\\s+(\\d+) \\(0x" line)
           (when gens
             (push (cons type size) (third (first gens))))))
        ((cl-ppcre:register-groups-bind ((#'parse-integer allocated size))
             ("^Total allocated (\\d+) .*total size (\\d+) " line)
           (setf total-allocated allocated
                 total-size size)))))
    (values
     (loop for (gen total alist) in (nreverse gens)
           collect (list gen total (nreverse alist)))
     total-allocated
     total-size)))

(defun format-mb (bytes)
  (format nil "~,1f" (/ bytes (* 1024 1024))))

(defun room-table (&key (stream *standard-output*))
  "Like (room :full), but prints the per-generation allocation as a
table with a column for each allocation type. All sizes are in MB."
  (multiple-value-bind (gens total-allocated total-size)
      (parse-room-output
       (with-output-to-string (*standard-output*)
         (room :full)))
    (let* ((types (remove-duplicates
                   (loop for (nil nil alist) in gens
                         append (mapcar #'car alist))
                   :test #'string=
                   :from-end t))
           (headers (append (list "Gen") types (list "Total")))
           (rows (append
                  (loop for (gen total alist) in gens
                        collect (append
                                 (list (format nil "~d" gen))
                                 (loop for type in types
                                       for size = (a:assoc-value alist type :test #'string=)
                                       collect (if size (format-mb size) "-"))
                                 (list (format-mb total))))
                  (list
                   (append
                    (list "All")
                    (loop for type in types
                          collect (format-mb
                                   (loop for (nil nil alist) in gens
                                         sum (or (a:assoc-value alist type :test #'string=) 0))))
                    (list (format-mb
                           (loop for (nil total) in gens sum total)))))))
           (widths (loop for i from 0 below (length headers)
                         collect (loop for row in (cons headers rows)
                                       maximize (length (elt row i))))))
      (flet ((print-row (row)
               (loop for cell in row
                     for width in widths
                     for first = t then nil
                     do (format stream (if first "~va" "  ~v@a") width cell))
               (terpri stream))
             (print-separator ()
               (format stream "~a~%"
                       (make-string (+ (reduce #'+ widths)
                                       (* 2 (1- (length widths))))
                                    :initial-element #\-))))
        (format stream "All sizes in MB~%~%")
        (print-row headers)
        (print-separator)
        (dolist (row (butlast rows))
          (print-row row))
        (print-separator)
        (print-row (car (last rows))))
      (when total-allocated
        (format stream "~%Total allocated ~a MB, total size ~a MB~%"
                (format-mb total-allocated)
                (format-mb total-size)))
      (values))))

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
