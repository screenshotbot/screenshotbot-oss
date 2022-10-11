;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/lru-cache
  (:use #:cl)
  (:import-from #:easy-macros
                #:def-easy-macro)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:lru-cache
   #:with-cache-file))
(in-package :util/lru-cache)

(defclass lru-cache ()
  ((dir :initarg :dir
        :reader dir)
   (lock :initform (bt:make-lock)
         :reader lock)
   (queue-head :initform nil
               :accessor queue-head)
   (queue-tail :initform nil
               :accessor queue-tail)
   (queue-count :initform 0
                :accessor queue-count)
   (queue-length :initform 0
                 :accessor queue-length)
   (cache-size :initform 0
               :accessor cache-size
               :documentation "The size of the cache in bytes")
   (cons-map :initform (make-hash-table :test #'equal)
             :reader cons-map))
  (:documentation "An LRU cache for items stored on disk"))

(defclass item ()
  ((key :initarg :key
        :reader item-key)
   (size :initarg :size
         :reader item-size)))

(defmethod file-atime ((self lru-cache) file)
  (error "unimpl"))

(defmethod read-all-files ((self lru-cache) directory)
  (let (ret)
    (labels ((dfs (dir parts)
              (loop for item in (fad:list-directory dir
                                                    :follow-symlinks nil)
                    do
                       (cond
                         ((path:-d item)
                          (dfs item (list*
                                     (car (last (pathname-directory item)))
                                     parts)))
                         (t
                          (push
                           (cons
                            (file-atime self item)
                            (make-instance
                             'item
                             :key (str:join "/"
                                            (reverse
                                             (list*
                                              (format nil "~a.~a"
                                                      (pathname-name item)
                                                      (pathname-type item))
                                              parts)))
                             :size (trivial-file-size:file-size-in-octets item)))
                           ret))))))
     (dfs directory nil)
     ret)))

(defmethod initialize-instance :after ((self lru-cache)
                                       &key dir
                                       &allow-other-keys)
  (let ((files (read-all-files self dir)))
    (setf (queue-head self)
          (mapcar #'cdr
                  (sort files #'< :key #'car)))
    (loop for item in (queue-head self)
          do (incf-cache-size self item))
    (reset-state self)))

(defmethod incf-cache-size ((self lru-cache)
                            (item item))
  (incf (cache-size self)
        (item-size item)))

(defmethod decf-cache-size ((self lru-cache)
                            (item item))
  (decf (cache-size self)
        (item-size item)))


(defmethod reset-state ((self lru-cache))
  (setf (queue-tail self)
        (last (queue-head self)))
  (loop for cons on (queue-head self)
        do
           (setf (gethash (item-key (car cons)) (cons-map self))
                 cons))
  (let ((len (length (queue-head self))))

    (setf (queue-length self) len
          (queue-count self) len)))

(def-easy-macro with-cache-file (&binding pathname cache key &fn fn)
  (assert key)
  (let ((pathname (path:catfile (dir cache) key)))
   (unwind-protect
        (funcall fn pathname)
     (bump-key cache pathname key))))

(defun bump-key (cache pathname key)
  (bt:with-lock-held ((lock cache))
    ;; remove the item if it exists in the queue
    (let ((prev-cons (gethash key (cons-map cache))))
      (when prev-cons
        (let ((item (car prev-cons)))
          (setf (car prev-cons) nil)
          (remhash key (cons-map cache))
          (decf (queue-count cache))
          (decf-cache-size cache item))))
    (let* ((item (make-instance 'item
                                :key key
                                :size (or
                                       (ignore-errors
                                        (trivial-file-size:file-size-in-octets pathname))
                                       0)))
           (new-tail (cons
                      item
                      nil)))
      (incf-cache-size cache item)
      (setf (gethash key (cons-map cache)) new-tail)
      (cond
        ((queue-tail cache)
         (setf (cdr (queue-tail cache)) new-tail)
         (setf (queue-tail cache) new-tail)
         (incf (queue-length cache))
         (incf (queue-count cache)))
        (t
         (setf (queue-head cache) new-tail)
         (setf (queue-tail cache) new-tail)
         (reset-state cache)))
      (maybe-trim-queue cache))))

(defmethod maybe-trim-queue ((cache lru-cache))
  (when (> (queue-length cache)
           (max
            10
            (* 2 (queue-count cache))))
    (setf (queue-head cache)
          (remove-if #'null (queue-head cache)))
    (reset-state cache)))
