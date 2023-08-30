;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

;;;; Maintains a persistent mapping from screenshot-keys to images.

(defpackage :screenshotbot/model/screenshot-map
  (:use #:cl)
  (:import-from #:bknr.datastore
                #:class-instances
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
  (:import-from #:screenshotbot/model/company
                #:company)
  (:import-from #:screenshotbot/events
                #:push-event)
  (:import-from #:screenshotbot/user-api
                #:company-name)
  (:import-from #:util/cron
                #:def-cron)
  (:export
   #:screenshot-map
   #:screenshot-map-as-list
   #:make-set
   #:to-map
   #:to-list
   #:make-screenshot-map))

(in-package :screenshotbot/model/screenshot-map)

(defparameter *lookback-count* 5
  "The number of recent screenshot-maps to look into to determine the
  best parent for a new map. TODO: We could use a precomputed
  _filtered_ map to actually do the comparisons, which would avoid the
  cost if the lookback is very large.")

(defparameter *max-chain-cost-factor* 50
  "Let's call this f. If adding a map to a chain, causes the chain cost
of the map to go beyond f*N, where N is the size of the map, then we
will not consider it as a possibility.")

(defvar *to-list-cache* (trivial-garbage:make-weak-hash-table
                         :weakness :value
                         #+sbcl
                         :synchronized #+sbcl t))

(defconstant +inf+ 1000000)

(with-class-validation
  (defclass screenshot-map (store-object)
    ((%channel :initarg :channel
               :reader screenshot-map-channel
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
          :initform nil)
     (chain-cost :transient t
                 :initform nil))
    (:metaclass persistent-class)))

(defmethod screenshot-map-to-list ((self screenshot-map))
  (slot-value self 'screenshots))

(defun memoized-reduce (fn map initial-value slot &optional (callback #'identity))
  "This is written in CPS form to avoid deep recursions."
  (labels ((%memoized-reduce (map &optional (callback #'identity))
             (cond
               ((null map) (funcall callback initial-value))
               (t
                (cond
                  ((slot-value map slot)
                   (funcall callback (slot-value map slot)))
                  (t
                   (%memoized-reduce (previous map)
                                     (lambda (previous-value)
                                       (funcall callback
                                                (setf (slot-value map slot)
                                                      (funcall fn map previous-value)))))))))))
    (%memoized-reduce map callback)))

(defun chain-cost (map)
  (memoized-reduce (lambda (this cost)
                     (+ (length (screenshots this))
                        (length (deleted this))
                        cost))
                   map
                   0
                   'chain-cost))

(defun make-set (list &optional (map (fset:empty-map)))
  (cond
    ((null list)
     map)
    (t
     (make-set (cdr list)
               (fset:with map (screenshot-key (first list))
                          (screenshot-image (first list)))))))

(defmethod to-map ((self screenshot-map))
  (memoized-reduce (lambda (self previous-set)
                     (let ((previous-set (fset:restrict-not
                                          previous-set
                                          (fset:convert 'fset:set
                                                        (deleted self)))))
                       (fset:map-union
                        previous-set
                        (make-set
                         (screenshots self)))))
                   self
                   (fset:empty-map)
                   'map))

(defmethod to-list ((self screenshot-map))
  (util:or-setf
   (gethash self *to-list-cache*)
   (loop for (key . image) in (fset:convert 'list (to-map self))
         collect (make-screenshot :key key :image image))))

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

(defun compute-cost (this-map prev-map)
  ;; This next step is computing the cost of the delta needed to
  ;; represent the difference between this-map and prev-map. This is
  ;; |A-B| + |D(B) - D(A)|, where D(X) is the domain of
  ;; X. fset:map-difference returns two maps
  (multiple-value-bind (diff-1 diff-2)
      (fset:map-difference-2 this-map prev-map)
    (+ (fset:size diff-1)
       (fset:size diff-2)
       ;; If the value for a given key is changed, then it will be
       ;; present in both |A-B| and |B-A|, so we need to account for
       ;; that.
       (-
        (fset:size (fset:intersection
                    (fset:domain diff-1)
                    (fset:domain diff-2)))))))

(defun pick-best-existing-map (channel screenshots)
  "Returns two values: the best previous map, or NIL if none, and the
  size of the delta between the previous map and this new map."
  (let ((this-map (make-set screenshots)))
    (labels ((find-best-in (potentials)
               (cond
                 ((null potentials)
                  (values nil +inf+))
                 (t
                  (destructuring-bind (this . rest) potentials
                    (let ((cost (compute-cost this-map (to-map this))))
                      (cond
                        ((zerop cost)
                         ;; Short circuit, we don't need to find a better map
                         (values this cost))
                        (t
                         (multiple-value-bind (next-best next-best-cost)
                             (find-best-in rest)
                           (cond
                             ((and
                               (< cost next-best-cost)
                               (< cost (fset:size this-map))
                               (<= (+ cost (chain-cost this))
                                   (* *max-chain-cost-factor* (fset:size this-map))))
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
      (prev
       (make-from-previous screenshots
                           prev
                           channel))
      (t
       (make-instance 'screenshot-map
                      :channel channel
                      :screenshots screenshots)))))

(defun build-usage-map ()
  (labels ((build (rest result)
             (cond
               ((not rest)
                result)
               (t
                (let* ((this (car rest))
                       (count (+ (length (screenshots this))
                                 (length (deleted this))))
                       (company (ignore-errors (company (screenshot-map-channel this)))))
                  (cond
                    (company
                     (build (cdr rest)
                            (fset:with
                             result
                             company
                             (+ count (fset:lookup result company)))))
                    (t
                     (build (cdr rest) result))))))))
    (build (class-instances 'screenshot-map)
           (fset:empty-map 0))))

;; (build-usage-map)

(defun push-usage-map ()
  (fset:do-map (key val (build-usage-map))
    (when (> val 1000)
      (push-event :screenshot-map.usage
                  :company (company-name key)
                  :cost val))))

(def-cron push-usage-map (:step-hour 1)
  (push-usage-map))
