;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/model/commit-graph-v2
  (:use #:cl)
  (:import-from #:bknr.datastore
                #:decode
                #:encode
                #:store-subsystem-snapshot-pathname
                #:close-subsystem
                #:snapshot-subsystem-async
                #:restore-subsystem)
  (:import-from #:util/store/store
                #:defsubsystem))
(in-package :screenshotbot/model/commit-graph-v2)

(defvar *map* (fset:empty-map)
  "A map from company -> { commit -> parents }.")

(defclass commit-graph-subsystem ()
  ())

(defmethod restore-subsystem ((store bknr.datastore:store) (self commit-graph-subsystem) &key until)
  (declare (ignore until))
  (let ((pathname (store-subsystem-snapshot-pathname store self)))
    (cond
      ((path:-e pathname)
       (with-open-file (input pathname
                              :direction :input
                              :element-type '(unsigned-byte 8))
         (let ((version (decode input)))
           (assert (= 1 version)))
         (setf *map* (decode input))))
      (t
       (setf *map* (fset:empty-map))))))

(defmethod snapshot-subsystem-async ((store bknr.datastore:store) (self commit-graph-subsystem))
  (let ((map *map*)
        (pathname (store-subsystem-snapshot-pathname store self)))
    (lambda ()
      (with-open-file (output pathname
                              :direction :output
                              :element-type '(unsigned-byte 8)
                              :if-exists :supersede)
        (encode 1 output) ;; version
        (encode map output)))))

(defmethod close-subsystem :after ((store bknr.datastore:store) (self commit-graph-subsystem))
  (setf *map* (fset:empty-map)))

(defsubsystem commit-graph-subsystem :priority 30)
