;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(uiop:define-package :screenshotbot/model/screenshot
  (:use #:cl
        #:alexandria
        #:screenshotbot/screenshot-api)
  (:import-from #:bknr.datastore
                #:with-transaction
                #:decode
                #:decode-object
                #:encode
                #:encode-object
                #:persistent-class
                #:store-object
                #:delete-object
                #:unique-index)
  (:import-from #:screenshotbot/user-api
                #:recorder-previous-run
                #:screenshot-name)
  (:import-from #:screenshotbot/report-api
                #:screenshot-lang
                #:screenshot-device)
  (:import-from #:screenshotbot/model/channel
                #:channel-promoted-runs)
  (:import-from #:screenshotbot/model/recorder-run
                #:run-screenshot-map
                #:active-run
                #:master-branch
                #:recorder-run
                #:screenshot-get-canonical
                #:recorder-run-screenshots)
  (:import-from #:screenshotbot/model/image
                #:base-image-comparer
                #:find-image-by-oid
                #:image=
                #:image-metadata
                #:%with-local-image
                #:rect-as-list)
  (:import-from #:util/store
                #:with-class-validation
                #:def-store-local)
  (:import-from #:screenshotbot/model/company
                #:company)
  (:import-from #:util/object-id
                #:oid
                #:oid-p)
  (:import-from #:screenshotbot/model/screenshot-key
                #:fake-screenshot-key
                #:screenshot-masks
                #:screenshot-key
                #:ensure-screenshot-key)
  (:import-from #:alexandria
                #:when-let)
  (:import-from #:screenshotbot/model/image-comparer
                #:make-image-comparer)
  (:import-from #:screenshotbot/model/screenshot-map
                #:to-map)
  (:export
   #:constant-string
   #:get-constant
   #:screenshot-masks
   #:screenshot-name
   #:screenshot-device
   #:screenshot-image
   #:screenshot-lang
   #:make-screenshot
   #:screenshot
   #:get-screenshot-history))
(in-package :screenshotbot/model/screenshot)


;; after implementing this, I realized, it's probably not really
;; needed. Most of my RAM usage is probably from the JVM. I had about
;; 600k screenshot objects, so probably less than 60MB of it would be
;; the names.
(with-class-validation
  (defclass constant-string (store-object)
    ((str :type string
          :initarg :str
          :index-initargs (:test #'equal)
          :index-type unique-index
          :index-reader constant-string-with-str
          :reader constant-string-string))
    (:metaclass persistent-class)))

(let ((lock (bt:make-lock)))
  (defmethod get-constant ((str string))
    (bt:with-lock-held (lock)
     (or
      (constant-string-with-str str)
      (make-instance 'constant-string :str str)))))

(def-store-local *screenshot-cache-v2* (make-hash-table :test 'equal))

(defclass abstract-screenshot ()
  ())

(with-class-validation
  (defclass screenshot (store-object abstract-screenshot)
    ((name :type string
           :initarg :name
           :accessor %screenshot-name)
     (recorder-run
      :initarg :run
      :accessor screenshot-run
      :documentation "DEPRECATED")
     (lang
      :initarg :lang
      :initform nil
      :accessor screenshot-lang)
     (device
      :initarg :device
      :initform nil
      :accessor screenshot-device)
     (image
      :initarg :image
      :initform nil
      :reader screenshot-image)
     (masks
      :initarg :masks
      :initform nil
      :accessor screenshot-masks))
    (:metaclass persistent-class)))

(defclass lite-screenshot (abstract-screenshot)
  ((%screenshot-key :initarg :screenshot-key
                   :reader screenshot-key)
   (%image-oid :initarg :image-oid
               :reader image-oid))
  (:documentation "A lightweight screenshot"))

(defmethod screenshot-key ((screenshot screenshot))
  (ensure-screenshot-key :name (screenshot-name screenshot)
                         :lang (screenshot-lang screenshot)
                         :device (screenshot-device screenshot)
                         :masks (screenshot-masks screenshot)))

(defmethod fset:compare ((one lite-screenshot) (two lite-screenshot))
  (fset:compare-slots
   one two
   #'screenshot-key
   #'image-oid))

(defmethod screenshot-name ((self lite-screenshot))
  (screenshot-name  (screenshot-key self)))

(defmethod screenshot-lang ((self lite-screenshot))
  (screenshot-lang (screenshot-key self)))

(defmethod screenshot-device ((self lite-screenshot))
  (screenshot-device (screenshot-key self)))

(defmethod screenshot-masks ((self lite-screenshot))
  (screenshot-masks (screenshot-key self)))

(defmethod screenshot-image ((self lite-screenshot))
  (when-let ((oid (image-oid self)))
   (find-image-by-oid oid)))

(defmethod encode-object ((self lite-screenshot) stream)
  (bknr.datastore::%write-tag #\S stream)
  (encode (screenshot-key self) stream)
  (encode (image-oid self) stream))

(defmethod decode-object ((tag (eql #\S)) stream)
  (let ((key (decode stream))
        (oid (decode stream)))
   (make-instance 'lite-screenshot
                  :screenshot-key key
                  :image-oid oid)))

(defmethod screenshot-image :around ((self screenshot))
  (let ((obj (call-next-method self)))
    (cond
      ((oid-p obj)
       (find-image-by-oid obj))
      (t
       obj))))

(defmethod company ((self screenshot))
  (company (screenshot-image self)))

(defclass fake-screenshot (abstract-screenshot)
  ((name :initarg :name
         :accessor %screenshot-name)
   (recorder-run
    :initarg :run
    :accessor screenshot-run)
   (lang
    :initarg :lang
    :initform nil
    :accessor screenshot-lang)
   (device
    :initarg :device
    :initform nil
    :accessor screenshot-device)
   (image
    :initarg :image
    :initform nil
    :accessor screenshot-image)
   (masks
    :initarg :masks
    :initform nil
    :accessor screenshot-masks))
  (:documentation "in memory screenshot object, used for canonical computation only"))

(defun screenshot-cache-key (screenshot)
  (cons
   (loop for x in (screenshot-masks screenshot)
         appending
         (rect-as-list x))
   (mapcar (lambda (x) (funcall x screenshot))
           (list 'screenshot-name
                  'screenshot-lang
                  'screenshot-device
                  'screenshot-image))))

;;(ensure-slot-boundp (store-objects-with-class 'screenshot) 'image)

(let ((lock (bt:make-lock)))
  (symbol-macrolet ((place (gethash (screenshot-cache-key screenshot) *screenshot-cache-v2*)))
    (defun screenshot-add-to-cache (screenshot)
      (check-type screenshot screenshot)
      (bt:with-lock-held (lock)
        (or place
            (setf place
                  screenshot))))
    (defmethod bknr.datastore::destroy-object ((screenshot screenshot))
      (bt:with-lock-held (lock)
        (when (eql place screenshot)
          (setf place nil)))
      (call-next-method))

    (defun screenshot-get-canonical (screenshot)
      place)))


(defmethod bknr.datastore:initialize-transient-instance :after ((screenshot screenshot))
  (screenshot-add-to-cache screenshot))

;; (mapc 'screenshot-add-to-cache (bknr.datastore:store-objects-with-class 'screenshot))


(defmethod screenshot-name ((screenshot abstract-screenshot))
  (let ((name (%screenshot-name screenshot)))
    (etypecase name
      (constant-string (constant-string-string name))
      (string name))))

(defun make-screenshot (&rest args &key image key &allow-other-keys)
  (let ((screenshot-key
          (cond
            (key key)
            (t
             (apply
              #'ensure-screenshot-key
              (remove-from-plist args :image))))))
    (make-instance 'lite-screenshot
                   :screenshot-key screenshot-key
                   :image-oid
                   (when image
                    (oid image :stringp nil)))))


(defmethod can-view ((screenshot screenshot) user)
  (can-view (screenshot-image screenshot) user))

(defmethod %with-local-image ((screenshot abstract-screenshot) fn)
  (%with-local-image (screenshot-image screenshot) fn))

(defun find-in-run (run screenshot-name)
  (when run
    (let ((map (to-map (run-screenshot-map run))))
      (multiple-value-bind (rank containsp)
          (fset:rank map (make-instance 'fake-screenshot-key
                                        :name screenshot-name))
        (flet ((from-rank (rank)
                 (when (< rank (fset:size map))
                   (multiple-value-bind (key value)
                       (fset:at-rank map rank)
                     (when (equal (screenshot-name key) screenshot-name)
                       (make-screenshot
                        :key key
                        :image value))))))
         (cond
           (containsp
            (from-rank rank))
           (t
            (from-rank (1+ rank)))))))))

(defun get-screenshot-history (channel screenshot-name &key (iterator nil))
  (let* ((branch (master-branch channel))
         (run (active-run channel branch)))
    (flet ((find-by-image (run screenshot)
             (when run
               (loop for s in (recorder-run-screenshots run)
                     if (image=
                         ;; We are doing the fast comparison here,
                         ;; since this is only for history purposes.
                         (make-instance 'base-image-comparer)
                         (screenshot-image s)
                         (screenshot-image screenshot)
                         nil)
                       return s))))
      (let ((screenshot (find-in-run run screenshot-name)))
        (labels ((iterator (run screenshot)
                   (when run
                     (let* ((prev-run (recorder-previous-run run))
                            (image-comparer
                              (make-image-comparer run))
                            (prev-screenshot
                              (when screenshot
                                (or
                                 (find-in-run prev-run
                                              (screenshot-name screenshot))
                                 (find-by-image prev-run screenshot)))))
                       (flet ((respond (prev-screenshot)
                                (let ((run run)
                                      (screenshot screenshot))
                                  (values (list screenshot run prev-screenshot)
                                          (lambda ()
                                            (iterator prev-run prev-screenshot))))))
                         (cond
                           ((not screenshot)
                            ;; The screenshot didn't exist when the
                            ;; channel was originally created, so we
                            ;; reached the end of its history.
                            (values nil nil))
                           ((not prev-screenshot)
                            (respond nil))
                           ((and
                             (string= (screenshot-name screenshot)
                                      (screenshot-name prev-screenshot))
                             (image=
                              image-comparer
                              (screenshot-image screenshot)
                              (screenshot-image prev-screenshot)
                              ;; TODO: should we use masks here?
                              ;; Probably not.
                              nil))
                            ;; Tail call optimize the next call
                            (iterator prev-run prev-screenshot))
                           (t
                            ;; in this, we found a difference between
                            ;; the current and the next screenshot.
                            (respond prev-screenshot))))))))

          (cond
            (iterator
             (lambda ()
              (iterator run screenshot)))
            (t
             (let ((iter (lambda () (iterator run screenshot))))
               (labels ((to-list (iter &optional res)
                          (cond
                            (iter
                             (multiple-value-bind (next next-iter) (funcall iter)
                               (to-list next-iter (list* next res))))
                            (t
                             (remove-if #'null
                              (mapcar #'first
                                      (reverse res)))))))
                 (to-list iter))))))))))

(defmethod image-metadata ((self abstract-screenshot))
  (image-metadata (screenshot-image self)))


(defun make-screenshot-from-key (key image)
  (make-instance 'fake-screenshot
                 :name (screenshot-name key)
                 :lang (screenshot-lang key)
                 :device (screenshot-device key)
                 :masks (screenshot-masks key)
                 :image image))

(defun make-key-from-screenshot (screenshot)
  (ensure-screenshot-key
   :name (screenshot-name screenshot)
   :lang (screenshot-lang screenshot)
   :device (screenshot-device screenshot)
   :masks (screenshot-masks screenshot)))

(defun remake-screenshot (screenshot)
  (make-screenshot
   :name (screenshot-name screenshot)
   :lang (screenshot-lang screenshot)
   :device (screenshot-device screenshot)
   :masks (screenshot-masks screenshot)
   :image (screenshot-image screenshot)))
