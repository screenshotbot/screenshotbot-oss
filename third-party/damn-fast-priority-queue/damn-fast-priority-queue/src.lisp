;;;; damn-fast-priority-queue.lisp

(defpackage #:damn-fast-priority-queue
  (:use #:cl)
  (:shadow #:map)
  (:export #:queue #:make-queue #:copy-queue
           #:enqueue #:dequeue #:peek #:size #:trim #:map #:do-queue
           #:queue-size-limit-reached
           #:queue-size-limit-reached-queue #:queue-size-limit-reached-object))

(in-package #:damn-fast-priority-queue)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Read-time variables

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *optimize-qualities*
    #+real-damn-fast-priority-queue
    ;; Good luck.
    `(optimize (speed 3) (debug 0) (safety 0) (space 0) (compilation-speed 0))
    #-real-damn-fast-priority-queue
    `(optimize (speed 3))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Type definitions

(deftype data-type () 't)

(deftype data-vector-type () '(simple-array data-type (*)))

(deftype prio-type () '(unsigned-byte 32))

(deftype prio-vector-type () '(simple-array prio-type (*)))

(deftype extension-factor-type () '(integer 2 256))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Structure definition

(declaim (inline %make %data-vector %prio-vector %size %extension-factor))

(defstruct (queue (:conc-name #:%) (:constructor %make)
                  (:predicate nil) (:copier nil))
  (data-vector (make-array 256 :element-type 'data-type) :type data-vector-type)
  (prio-vector (make-array 256 :element-type 'prio-type) :type prio-vector-type)
  (size 0 :type alexandria:array-length)
  (extension-factor 2 :type extension-factor-type)
  (extend-queue-p t :type boolean))

(declaim (inline make-queue copy-queue))

(declaim (ftype (function
                 (&optional alexandria:array-index extension-factor-type boolean)
                 (values queue &optional))
                make-queue))
(defun make-queue (&optional
                     (initial-storage-size 256)
                     (extension-factor 2)
                     (extend-queue-p t))
  (declare (type extension-factor-type extension-factor))
  (declare #.*optimize-qualities*)
  (%make :extension-factor extension-factor
         :data-vector (make-array initial-storage-size
                                  :element-type 'data-type)
         :prio-vector (make-array initial-storage-size
                                  :element-type 'prio-type)
         :extend-queue-p extend-queue-p))

(defmethod print-object ((object queue) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "(~D)" (%size object))))

(declaim (ftype (function (queue) (values queue &optional)) copy-queue))
(defun copy-queue (queue)
  (declare (type queue queue))
  (declare #.*optimize-qualities*)
  (%make :extension-factor (%extension-factor queue)
         :size (%size queue)
         :extend-queue-p (%extend-queue-p queue)
         :data-vector (copy-seq (%data-vector queue))
         :prio-vector (copy-seq (%prio-vector queue))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Enqueueing

(declaim (inline heapify-upwards enqueue))

(declaim (ftype (function (data-vector-type prio-vector-type alexandria:array-length)
                          (values null &optional))
                heapify-upwards))
(defun heapify-upwards (data-vector prio-vector index)
  (declare (type data-vector-type data-vector))
  (declare (type prio-vector-type prio-vector))
  (declare (type alexandria:array-length index))
  (declare #.*optimize-qualities*)
  (do ((child-index index parent-index)
       (parent-index (ash (1- index) -1) (ash (1- parent-index) -1)))
      ((= child-index 0))
    (let ((child-priority (aref prio-vector child-index))
          (parent-priority (aref prio-vector parent-index)))
      (cond ((< child-priority parent-priority)
             (rotatef (aref prio-vector parent-index)
                      (aref prio-vector child-index))
             (rotatef (aref data-vector parent-index)
                      (aref data-vector child-index)))
            (t (return))))))

(declaim (ftype (function (queue t fixnum) (values null &optional)) enqueue))
(defun enqueue (queue object priority)
  (declare (type queue queue))
  (declare (type fixnum priority))
  (declare #.*optimize-qualities*)
  (symbol-macrolet ((data-vector (%data-vector queue))
                    (prio-vector (%prio-vector queue)))
    (let ((size (%size queue))
          (extension-factor (%extension-factor queue))
          (length (array-total-size data-vector)))
      (when (>= size length)
        (unless (%extend-queue-p queue)
          (error 'queue-size-limit-reached :queue queue :element object))
        (let ((new-length (max 1 (mod (* length extension-factor)
                                      (ash 1 64)))))
          (declare (type alexandria:array-length new-length))
          (when (<= new-length length)
            (error "Integer overflow while resizing array: new-length ~D is ~
                    smaller than old length ~D" new-length length))
          (setf data-vector (adjust-array data-vector new-length)
                prio-vector (adjust-array prio-vector new-length))))
      (setf (aref data-vector size) object
            (aref prio-vector size) priority)
      (heapify-upwards data-vector prio-vector (%size queue))
      (incf (%size queue))
      nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Dequeueing

(declaim (inline heapify-downwards dequeue))

(declaim (ftype (function (data-vector-type prio-vector-type alexandria:array-index)
                          (values null &optional))
                heapify-downwards))
(defun heapify-downwards (data-vector prio-vector size)
  (declare (type data-vector-type data-vector))
  (declare (type prio-vector-type prio-vector))
  (declare #.*optimize-qualities*)
  (let ((parent-index 0))
    (loop
      (let* ((left-index (+ (* parent-index 2) 1))
             (left-index-validp (< left-index size))
             (right-index (+ (* parent-index 2) 2))
             (right-index-validp (< right-index size)))
        (flet ((swap-left ()
                 (rotatef (aref prio-vector parent-index)
                          (aref prio-vector left-index))
                 (rotatef (aref data-vector parent-index)
                          (aref data-vector left-index))
                 (setf parent-index left-index))
               (swap-right ()
                 (rotatef (aref prio-vector parent-index)
                          (aref prio-vector right-index))
                 (rotatef (aref data-vector parent-index)
                          (aref data-vector right-index))
                 (setf parent-index right-index)))
          (declare (inline swap-left swap-right))
          (when (and (not left-index-validp)
                     (not right-index-validp))
            (return))
          (when (and left-index-validp
                     (< (aref prio-vector parent-index)
                        (aref prio-vector left-index))
                     (or (not right-index-validp)
                         (< (aref prio-vector parent-index)
                            (aref prio-vector right-index))))
            (return))
          (if (and right-index-validp
                   (<= (aref prio-vector right-index)
                       (aref prio-vector left-index)))
              (swap-right)
              (swap-left)))))))

(declaim (ftype (function (queue) (values t boolean &optional)) dequeue))
(defun dequeue (queue)
  (declare (type queue queue))
  (declare #.*optimize-qualities*)
  (if (= 0 (%size queue))
      (values nil nil)
      (let ((data-vector (%data-vector queue))
            (prio-vector (%prio-vector queue)))
        (multiple-value-prog1 (values (aref data-vector 0) t)
          (decf (%size queue))
          (let ((old-data (aref data-vector (%size queue)))
                (old-prio (aref prio-vector (%size queue))))
            (setf (aref data-vector 0) old-data
                  (aref prio-vector 0) old-prio))
          (heapify-downwards data-vector prio-vector (%size queue))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Introspection and maintenance

(declaim (inline peek size trim map))

(declaim (ftype (function (queue) (values t boolean &optional)) peek))
(defun peek (queue)
  (declare (type queue queue))
  (declare #.*optimize-qualities*)
  (if (= 0 (%size queue))
      (values nil nil)
      (values (aref (%data-vector queue) 0) t)))

(declaim (ftype (function (queue) (values alexandria:array-length &optional)) size))
(defun size (queue)
  (declare (type queue queue))
  (declare #.*optimize-qualities*)
  (%size queue))

(declaim (ftype (function (queue) (values null &optional)) trim))
(defun trim (queue)
  (declare (type queue queue))
  (declare #.*optimize-qualities*)
  (let ((size (%size queue)))
    (setf (%data-vector queue) (adjust-array (%data-vector queue) size)
          (%prio-vector queue) (adjust-array (%prio-vector queue) size))
    nil))

(declaim (ftype (function (queue (function (t) t)) (values null &optional))
                map))
(defun map (queue function)
  (loop repeat (%size queue)
        for data across (%data-vector queue)
        do (funcall function data)))

(defmacro do-queue ((object queue &optional result) &body body)
  (multiple-value-bind (forms declarations) (alexandria:parse-body body)
    (alexandria:once-only (queue)
      `(loop repeat (%size ,queue)
             for ,object across (%data-vector ,queue)
             do (locally ,@declarations (tagbody ,@forms))
             finally (return ,result)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Conditions

(defun report-queue-size-limit-reached (condition stream)
  (let ((queue (queue-size-limit-reached-queue condition))
        (element (queue-size-limit-reached-object condition)))
    (format stream "Size limit (~D) reached for non-extensible ~
                    queue ~S while trying to enqueue element ~S onto it."
            (length (%data-vector queue)) queue element)))

(define-condition queue-size-limit-reached (error)
  ((%queue :reader queue-size-limit-reached-queue :initarg :queue)
   (%object :reader queue-size-limit-reached-object :initarg :element))
  (:default-initargs :queue (alexandria:required-argument :queue)
                     :object (alexandria:required-argument :object))
  (:report report-queue-size-limit-reached))
