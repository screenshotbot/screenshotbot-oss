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
                #:persistent-class
                #:store-object
                #:delete-object
                #:unique-index)
  (:import-from #:screenshotbot/user-api
                #:screenshot-name)
  (:import-from #:screenshotbot/report-api
                #:screenshot-lang
                #:screenshot-device)
  (:import-from #:screenshotbot/model/channel
                #:channel-promoted-runs)
  (:import-from #:screenshotbot/model/recorder-run
                #:screenshot-get-canonical
                #:recorder-run-screenshots)
  (:import-from #:screenshotbot/model/image
                #:%with-local-image
                #:rect-as-list)
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
(defclass constant-string (store-object)
  ((str :type string
        :initarg :str
        :index-initargs (:test #'equal)
        :index-type unique-index
        :index-reader constant-string-with-str
        :reader constant-string-string))
  (:metaclass persistent-class))

(let ((lock (bt:make-lock)))
  (defmethod get-constant ((str string))
    (bt:with-lock-held (lock)
     (or
      (constant-string-with-str str)
      (make-instance 'constant-string :str str)))))

(defvar *screenshot-cache* (make-hash-table :test 'equal))

(defclass abstract-screenshot ()
  ())

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
    :accessor screenshot-image)
   (masks
    :initarg :masks
    :initform nil
    :accessor screenshot-masks))
  (:metaclass persistent-class))

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
  (symbol-macrolet ((place (gethash (screenshot-cache-key screenshot) *screenshot-cache*)))
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

;; (mapc 'screenshot-add-to-cache (store-objects-with-class 'screenshot))


(defmethod screenshot-name ((screenshot abstract-screenshot))
  (let ((name (%screenshot-name screenshot)))
    (etypecase name
      (constant-string (constant-string-string name))
      (string name))))

(defun make-screenshot (&rest args)
  (let* ((screenshot (apply 'make-instance 'fake-screenshot args))
         (canonical (screenshot-get-canonical screenshot)))
    (or canonical
        (apply 'make-instance 'screenshot args))))


(defmethod can-view ((screenshot screenshot) user)
  (can-view (screenshot-image screenshot) user))

(defmethod %with-local-image ((screenshot screenshot) fn)
  (%with-local-image (screenshot-image screenshot) fn))

(defun get-screenshot-history (channel screenshot-name)
  (let ((last-run nil)
        (result nil)
        (result-runs nil))
    (loop for run in (reverse (channel-promoted-runs channel))
          collect
          (let ((screenshots (recorder-run-screenshots run)))
            (loop for s in screenshots
                  if (string= screenshot-name (screenshot-name s))
                    do (progn
                         (cond
                           ((or
                             (and (not last-run) (not s))
                             (and last-run s
                                  (eql (screenshot-image last-run)
                                       (screenshot-image s))))
                            ;; do nothing, this outcome is same as last outcome
                            nil)
                           (t
                            (push s result)
                            (push run result-runs)))
                         (setf last-run s)))))
    (values result result-runs)))
