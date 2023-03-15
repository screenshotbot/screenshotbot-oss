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
                #:make-screenshot
                #:screenshot-image)
  (:import-from #:screenshotbot/model/screenshot-key
                #:screenshot-key)
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
                  :reader screenshots
                  :documentation "The list of screenhots that are added on top of the previous")
     (deleted :initarg :deleted
              :reader deleted
              :initform nil
              :documentation "The list of screenshot-keys that will be deleted from the previous.")
     (previous :initarg :previous
               :initform nil
               :reader previous)
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
   (let ((screenshots
           (make-set
            (screenshots self))))
     (let ((previous (previous self)))
       (cond
         (previous
          (let ((previous-set (to-map previous)))
            (let ((previous-set (fset:restrict-not
                                 previous-set
                                 (fset:convert 'fset:set
                                               (deleted self)))))
             (fset:map-union
              previous-set
              screenshots))))
         (t
          screenshots))))))

(defmethod make-from-previous (screenshots (previous screenshot-map))
  (let* ((map (make-set screenshots))
         (old-map (to-map previous))
         (added (fset:map-difference-2
                 map old-map))
         (deleted (fset:set-difference
                   (fset:domain old-map)
                   (fset:domain map))))
    (make-instance 'screenshot-map
                   :screenshots
                   (fset:reduce (lambda (list key val)
                                  (list*
                                   (make-screenshot
                                    :key key
                                    :image val)
                                   list))
                                added
                                :initial-value nil)
                   :deleted (fset:reduce (lambda (list key)
                                           (list* key list))
                                         deleted :initial-value nil)
                   :previous previous)))


(defun make-screenshot-map (channel screenshots)
  (let ((prev (car (last (screenshot-maps-for-channel channel)))))
    (let ((this-set (make-set screenshots)))
     (cond
       ((and
         prev
         (fset:equal?
          (to-map prev)
          this-set))
        prev)
       ((and
         prev
         (not (eql
               0
               (fset:size
                (fset:map-intersection
                 (to-map prev)
                 this-set)))))
        (make-from-previous screenshots
                            prev))
       (t
        (make-instance 'screenshot-map
                       :channel channel
                       :screenshots screenshots))))))
