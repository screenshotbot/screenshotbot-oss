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
  (:import-from #:util/lists
                #:head)
  (:export
   #:screenshot-map
   #:screenshot-map-as-list))
(in-package :screenshotbot/model/screenshot-map)

(defparameter *delta-factor* 0.5
  "If the delta of a new map with a previous map is above this factor,
  then we'll not parent it.")

(defparameter *lookback-count* 5
  "The number of recent screenshot-maps to look into to determine the
  best parent for a new map. TODO: We could use a precomputed
  _filtered_ map to actually do the comparisons, which would avoid the
  cost if the lookback is very large.")

(defconstant +inf+ 1000000)

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

(defmethod make-from-previous (screenshots (previous screenshot-map)
                               channel)
  (let* ((map (make-set screenshots))
         (old-map (to-map previous))
         (added (fset:map-difference-2
                 map old-map))
         (deleted (fset:set-difference
                   (fset:domain old-map)
                   (fset:domain map))))
    (make-instance 'screenshot-map
                   :channel channel
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

(defmethod make-from-previous (screenshots (previous null)
                               channel)
  (make-instance 'screenshot-map
                 :channel channel
                 :screenshots screenshots))

(defun pick-best-existing-map (channel screenshots)
  "Returns two values: the best previous map, or NIL if none, and the
  size of the delta between the previous map and this new map."
  (let ((this-map (make-set screenshots)))
    (labels ((compute-cost (prev)
               (let ((prev-map (to-map prev)))
                 ;; This next step is computing |A-B| + |B-A|, where A and B
                 ;; are the *set* of pairs.
                 (multiple-value-bind (diff-1 diff-2)
                     (fset:map-difference-2 this-map prev-map)
                   (+ (fset:size diff-1)
                      (fset:size diff-2)))))
             (find-best-in (potentials)
               (cond
                 ((null potentials)
                  (values nil +inf+))
                 (t
                  (destructuring-bind (this . rest) potentials
                    (let ((cost (compute-cost this)))
                      (cond
                        ((zerop cost)
                         ;; Short circuit, we don't need to find a better map
                         (values this cost))
                        (t
                         (multiple-value-bind (next-best next-best-cost)
                             (find-best-in rest)
                           (cond
                             ((< cost next-best-cost)
                              (values this cost))
                             (t
                              (values next-best next-best-cost))))))))))))
      (find-best-in (head (screenshot-maps-for-channel channel) *lookback-count*)))))

(defun make-screenshot-map (channel screenshots)
  (multiple-value-bind (prev delta-size)
      (pick-best-existing-map channel screenshots)
    (cond
      ((and prev (zerop delta-size))
       prev)
      ((and
        prev
        (< delta-size (* *delta-factor* (length screenshots))))
       (make-from-previous screenshots
                           prev
                           channel))
      (t
       (make-instance 'screenshot-map
                      :channel channel
                      :screenshots screenshots)))))
