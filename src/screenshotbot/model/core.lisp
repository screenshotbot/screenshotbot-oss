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
  (:import-from #:screenshotbot/model/api-key
                #:generate-api-key
                #:generate-api-secret)
  (:import-from #:util/lists
                #:with-batches)
  (:export
   #:has-created-at
   #:ensure-slot-boundp
   #:generate-api-key
   #:generate-api-secret
   #:%created-at
   #:non-root-object))
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

(deftransaction tx-ensure-slot-boundp (items slot value)
  (loop for item in items do
      (unless (slot-boundp item slot)
        (setf (slot-value item slot) value))))

(defun ensure-slot-boundp (item slot &key value)
  (let ((items (cond
                 ((listp item)
                  item)
                 ((symbolp item)
                  (bknr.datastore:class-instances item))
                 (t
                  (list item)))))
    (with-batches (items items :batch-size 1000)
      (tx-ensure-slot-boundp items slot value))))



(defun print-type-histogram ()
  (let* ((types (mapcar 'type-of (bknr.datastore:all-store-objects)))
         (names (remove-duplicates types)))
    (loop for name in names do
      (format t "~a: ~a~%" name (count name types)))))

(defclass non-root-object (store-object)
  ()
  (:metaclass persistent-class)
  (:documentation "An object that is safe to garbage collect if it's no longer reachable"))
