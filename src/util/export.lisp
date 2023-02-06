;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/export
  (:use #:cl)
  (:import-from #:bknr.datastore
                #:encode))
(in-package :util/export)

(defmethod export-slots (class (slot-name symbol))
  (loop for obj in (bknr.datastore:class-instances class)
        if (slot-boundp obj slot-name)
        collect (list
                 (bknr.datastore:store-object-id obj)
                 (slot-value obj slot-name))))

(defmethod export-slots (class (slot-name string))
  (loop for slot in (closer-mop:class-slots (find-class class))
        for slot-def-name = (closer-mop:slot-definition-name slot)
        if (string-equal slot-def-name slot-name)
          ;; Convert name to symbol :/
          return (export-slots class slot-def-name)))

(defmethod export-slots-to-file (class slot-name file)
  (with-open-file (stream file :direction :output
                               :element-type 'flex:octet)
    (encode
     (export-slots class slot-name)
     stream)))

#+nil
(export-slots-to-file
 'screenshotbot/bitbucket/promoter::pr-acceptable
 "send-task-args"
 "/home/arnold/builds/web/slots.obj")
