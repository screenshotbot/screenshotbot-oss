;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :core/ui/paginated
  (:use #:cl)
  (:import-from #:nibble
                #:nibble)
  (:export
   #:paginated))
(in-package :core/ui/paginated)

(markup:enable-reader)

(defun paginated (fn &key
                       (filter #'identity)
                       (empty-view)
                       (num 24)
                       (items nil)
                       (start-counter 0)
                       (iterator nil)
                       (pass-index-p nil))
  "Creates a paginated view. If no element matches, then we return
NIL, which can be used as a way of determining whether to render an
empty message."
  (assert (functionp fn))
  (let ((fn (if pass-index-p
                fn
                (lambda (name i)
                  (declare (ignore i))
                  (funcall fn name)))))
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
         (t
          (util/lists:head items num :filter filter)))
     (cond
       (this-page
        (let* ((load-more (nibble (:name :load-more)
                            (paginated fn :num num :items (if iterator nil rest)
                                          :start-counter (+ start-counter (length this-page))
                                          :iterator iterator
                                          :pass-index-p t
                                          :filter filter))))
          <div class= "row pb-4 load-more-container" >
            ,@(loop for page in this-page
                    for i from start-counter
                    collect (funcall fn page i))

          ,(when rest
             <div class= "col-12 d-flex justify-content-center">
             <button class= "btn btn-primary load-more-button" data-load-more=load-more >Load More</button>
             </div>)
          </div>))
       (t
        empty-view)))))
