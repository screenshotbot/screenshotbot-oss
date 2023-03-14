;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

;;;; Maintains a persistent mapping from screenshot-keys to images.

(defpackage :screenshotbot/model/screenshot-map
  (:use #:cl)
  (:import-from #:bknr.datastore
                #:store-object)
  (:import-from #:bknr.datastore
                #:persistent-class)
  (:import-from #:util/store
                #:with-class-validation)
  (:import-from #:bknr.indices
                #:hash-index)
  (:import-from #:screenshotbot/screenshot-api
                #:screenshot-key
                #:screenshot-image)
  (:export
   #:screenshot-map
   #:screenshot-map-as-list))
(in-package :screenshotbot/model/screenshot-map)

(with-class-validation
  (defclass screenshot-map (store-object)
    ((%channel :initarg :channel
               :index-type hash-index
               :index-reader screenshot-maps-for-channel)
     (screenshots :initarg :screenshots
                  :reader screenshots)
     (map :transient t
          :initform nil))
    (:metaclass persistent-class)))

(defmethod screenshot-map-to-list ((self screenshot-map))
  (slot-value self 'screenshots))

(defun make-set (list &optional (map (fset:empty-map)))
  (cond
    ((null list)
     map)
    (t
     (make-set (cdr list)
               (fset:with map (screenshot-key (first list))
                          (screenshot-image (first list)))))))

(defmethod to-map ((self screenshot-map))
  (util:or-setf
   (slot-value self 'map)
   (make-set
    (screenshots self))))

(defun build-from-best ())

(defun make-screenshot-map (channel screenshots)
  (let ((prev (car (last (screenshot-maps-for-channel channel)))))
    (cond
      ((and
        prev
        (fset:equal?
         (to-map prev)
         (make-set screenshots)))
       prev)
      (t
       (make-instance 'screenshot-map
                      :channel channel
                      :screenshots screenshots)))))
