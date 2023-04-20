;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/store/export
  (:nicknames :util/export)
  (:use #:cl)
  (:import-from #:bknr.datastore
                #:encode)
  (:import-from #:bknr.datastore
                #:decode)
  (:import-from #:bknr.datastore
                #:with-transaction))
(in-package :util/export)

(defmethod export-slots (class (slot-name symbol))
  (loop for obj in (bknr.datastore:class-instances class)
        if (slot-boundp obj slot-name)
        collect (list
                 (bknr.datastore:store-object-id obj)
                 (string slot-name)
                 (slot-value obj slot-name))))

(defmethod find-slot (class (slot-name string))
  (loop for slot in (closer-mop:class-slots (find-class class))
        for slot-def-name = (closer-mop:slot-definition-name slot)
        if (string-equal slot-def-name slot-name)
          ;; Convert name to symbol :/
          return slot-def-name))

(defmethod export-slots (class (slot-name string))
  (export-slots class (find-slot class slot-name)))

(defmethod export-slots-to-file (class slot-name file)
  (with-open-file (stream file :direction :output
                               :element-type 'flex:octet)
    (encode
     (export-slots class slot-name)
     stream)))

(defun import-slots (slots)
  (loop for (id slot-name res) in slots
        for obj = (bknr.datastore:store-object-with-id id)
        do (import-slot-for-obj obj
                                (find-slot (type-of obj) slot-name)
                                res)))

(defun import-slot-for-obj (obj slot-def-name res)
  (log:info "importing ~a ~a" obj slot-def-name)
  (restart-case
      (cond
        ((slot-boundp obj slot-def-name)
         (error "Slot already bound for ~a" obj))
        (t
         (with-transaction ()
           (setf (slot-value obj slot-def-name) res))))
    (continue ()
      nil)))

(defmethod import-slots-from-file (class slot-name file)
  (with-open-file (stream file :direction :input
                                :element-type 'flex:octet)
    (import-slots (decode stream))))


#+nil
(export-slots
 'screenshotbot/bitbucket/promoter::bitbucket-acceptable
 "send-task-args")
