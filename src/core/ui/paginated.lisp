;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :core/ui/paginated
  (:use #:cl)
  (:import-from #:nibble
                #:nibble)
  (:import-from #:alexandria
                #:remove-from-plist)
  (:export
   #:paginated))
(in-package :core/ui/paginated)

(named-readtables:in-readtable markup:syntax)

(defvar *filter-cache* (trivial-garbage:make-weak-hash-table
                        :weakness :value
                        :test #'equal
                        #+sbcl
                        :synchronized #+sbcl t))

(defun apply-map-filter (map filter)
  (util:or-setf
   (gethash (cons map filter) *filter-cache*)
   (cond
     ((eql #'identity filter)
      map)
     (t
      (fset:filter (lambda (x v)
                     (declare (ignore v))
                     (funcall filter x))
                   map)))))

(defun pagination-helper (&key
                           (filter #'identity)
                           (empty-view)
                           (num 24)
                           (items nil)
                           (start-counter 0)
                           (iterator nil)
                           renderer)
  "The renderer here is a function that takes three arguments: the list of objects to render,
 a lambda that calls pagination-helper on the remaining objects with
 the same arguments, and the start-counter of the first element.

For a map, the filter filters on keys, not on values.

The pagination-helper doesn't handle rendering on its own, for testability purposes."
  (multiple-value-bind (this-page rest)
      (cond
        (iterator
         (loop for i from 0 upto num
               for next = (funcall iterator)
               if next
                 collect next into results
               if (null next)
                 do (return (values results nil))
               finally
                  (return (values results t))))
        ((fset:map? items)
         (let ((items (apply-map-filter items filter)))
          (values
           (loop for i below num
                 for j from start-counter below (fset:size items)
                 collect (multiple-value-bind (key value)
                             (fset:at-rank items j)
                           (cons key value)))
           ;; We can use the same map for the rest, since we're
           ;; indexing by numbers.
           (when (< (+ start-counter num) (fset:size items))
             items))))
        (t
         (util/lists:head items num :filter filter)))
    (cond
      (this-page
       (let* ((load-more (when rest
                           (lambda ()
                             (pagination-helper
                              :num num :items (if iterator nil rest)
                              :start-counter (+ start-counter (length this-page))
                              :iterator iterator
                              :filter filter
                              :renderer renderer)))))
         (funcall renderer this-page load-more start-counter)))
      (t
       empty-view))))

(defun paginated (fn &rest args &key pass-index-p &allow-other-keys)
  "Creates a paginated view. If no element matches, then we return
NIL, which can be used as a way of determining whether to render an
empty message."
  (let ((fn (if pass-index-p
                fn
                (lambda (name i)
                  (declare (ignore i))
                  (funcall fn name)))))
   (apply #'pagination-helper
          :renderer (lambda (this-page load-more start-counter)
                      <div class= "row pb-4 load-more-container" >
                      ,@(loop for page in this-page
                              for i from start-counter
                              collect (funcall fn page i))

                      ,(when load-more
                         <div class= "col-12 d-flex justify-content-center">
                         <button class= "btn btn-primary load-more-button" data-load-more= (nibble (:name :load-more) (funcall load-more)) >Load More</button>
                         </div>)
                      </div>)
          (remove-from-plist args :pass-index-p))))
