;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/dashboard/paginated
  (:use #:cl)
  (:import-from #:nibble
                #:nibble)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:paginated))
(in-package :screenshotbot/dashboard/paginated)

(markup:enable-reader)

(defun paginated (fn &key
                       (filter #'identity)
                       (empty-view)
                       (num 24)
                       (items nil)
                       (iterator nil))
  "Creates a paginated view. If no element matches, then we return
NIL, which can be used as a way of determining whether to render an
empty message."
  (assert (functionp fn))
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
                                      :iterator iterator
                                         :filter filter))))
         <div class= "row pb-4 load-more-container" >
         ,@ (mapcar fn this-page)

         ,(when rest
            <div class= "col-12 d-flex justify-content-center">
            <button class= "btn btn-primary load-more-button" data-load-more=load-more >Load More</button>
            </div>)
         </div>))
      (t
       empty-view))))
