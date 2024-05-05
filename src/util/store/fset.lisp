;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/store/fset
  (:use #:cl)
  (:import-from #:bknr.datastore
                #:decode
                #:encode
                #:%read-tag
                #:%write-tag
                #:encode-object
                #:decode-object)
  (:import-from #:util/store/store
                #:object-neighbors))
(in-package :util/store/fset)

(defmethod decode-object ((code (eql #\F)) stream)
  (decode-fset-object (%read-tag stream)
                      stream))

(defmethod decode-fset-object ((code (eql #\m)) stream)
  (let ((length (decode stream)))
    (let ((res (fset:empty-map)))
      (labels ((read-len (length res)
                 (cond
                   ((= length 0)
                    res)
                   (t
                    (read-len (1- length)
                                 (fset:with
                                  res
                                  (decode stream)
                                  (decode stream)))))))
        (read-len length (fset:empty-map))))))

(defmethod encode-object ((map fset:map) stream)
  (%write-tag #\F stream)
  (%write-tag #\m stream)
  (encode (fset:size map) stream)
  (fset:do-map (key val map)
    (encode key stream)
    (encode val stream)))


(defmethod object-neighbors ((map fset:map))
  (let ((ret))
    (fset:do-map (key val map)
      (push key ret)
      (push val ret))
    ret))

(defmethod object-neighbors ((set fset:set))
  (fset:convert 'list set))
