;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(uiop:define-package :screenshotbot/model/core
  (:use #:cl #:alexandria #:util)
  (:import-from #:screenshotbot/user-api
                #:%created-at)
  (:import-from #:bknr.datastore
                #:persistent-class
                #:with-transaction
                #:store-object
                #:deftransaction)
  (:export
   #:has-created-at
   #:ensure-slot-boundp
   #:generate-api-key
   #:generate-api-secret
   #:%created-at))
(in-package :screenshotbot/model/core)

(defclass has-created-at (persistent-class)
  ())

(deftransaction
    set-created-at (obj val)
  (check-type obj store-object)
  (check-type val number)
  (setf (%created-at obj) val))

(defmethod make-instance :around ((sym has-created-at) &rest args)
  (let ((res (call-next-method)))
    (set-created-at res (get-universal-time))
    res))

(defun ensure-slot-boundp (item slot)
  (let ((items (if (listp item) item (list item))))
    (loop for item in items do
      (unless (slot-boundp item slot)
        (with-transaction ()
         (setf (slot-value item slot) nil))))))

(defun generate-random-string (length chars)
  (coerce (loop repeat length collect (aref chars (random (length chars))))
          'string))

(defun %all-alpha (&optional (from \#a) (to \#z))
  (let ((From (char-code from))
        (to (char-code to)))
    (assert (< from to))
   (loop for i from from to to collect
        (code-char i))))

(defun generate-api-key ()
  (generate-random-string 20 (concatenate 'string (%all-alpha #\A #\Z)
                                          (%all-alpha #\0 #\9))))

(Defun generate-api-secret ()
  (generate-random-string 40 (concatenate 'string
                                          (%all-alpha #\A #\Z)
                                          (%all-alpha #\a #\z)
                                          (%all-alpha #\0 #\9))))

(defun print-type-histogram ()
  (let* ((types (mapcar 'type-of (bknr.datastore:all-store-objects)))
         (names (remove-duplicates types)))
    (loop for name in names do
          (format t "~a: ~a~%" name (count name types)))))
