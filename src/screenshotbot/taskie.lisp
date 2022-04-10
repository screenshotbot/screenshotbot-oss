;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(uiop:define-package :screenshotbot/taskie
  (:use #:cl #:alexandria)
  (:import-from #:markup
                #:deftag)
  (:import-from #:screenshotbot/template
                #:mdi)
  (:import-from #:util
                #:oid
                #:object-with-oid)
  (:import-from #:bknr.datastore
                #:store-object-id)
  (:import-from #:nibble #:nibble)
  (:export
   #:taskie-list
   #:taskie-row
   #:taskie-page-title
   #:taskie-timestamp
   #:taskie-page-item
   #:timeago
   #:with-pagination))
(in-package :screenshotbot/taskie)

(markup:enable-reader)

(defun first-n (n x)
  (loop for i in x
        for j from 0 below n
        collect (nth j x)))

(deftag taskie-page-title (children &key title)
  <div class= "page-title-box">
  <h4 class= "page-title" >,(progn title)
    ,@children
  </h4>
  </div>)

(deftag taskie-page-item (children &key href)
  <li class= (format nil "page-item ~a" (unless href "disabled"))>
    <a class= "page-link" href=href >
      ,@children
    </a>
  </li>)

(deftag taskie-list (children &key empty-message items row-generator
                     next-link
                     prev-link)
  <markup:merge-tag>
  <div class= (format nil "card mb-0 taskie-list mt-3 mb-3 ~a"
                      (if items
                          "nonempty"
                          "empty")) >
    <div class="card-body">
      ,(unless items
         <p class= "text-muted" >,(progn empty-message) </p>)
      ,@ (loop for item in items collect
               (funcall row-generator item))
    </div> <!-- end card-body-->

  </div>
  ,(when (or prev-link next-link)
       <nav aria-label="Page navigation" class= "mt-3" >
         <ul class="pagination justify-content-center">
           <taskie-page-item href=prev-link >Previous</taskie-page-item>
           <taskie-page-item href=next-link >Next</taskie-page-item>
  </ul>
</nav>
)
           </markup:merge-tag>)

(let ((id-counter 0))
 (deftag taskie-row (children &key object)
   (let ((children (remove-if 'stringp children))
         (id-name (format nil "check-~a"(incf id-counter))))
     <div class="row justify-content-sm-between">
       <div class="col-sm-6 mb-1 mt-1 mb-sm-0">
         <div class="custom-control custom-checkbox">
           <input type="checkbox" class="recent-run-item custom-control-input"
                  id=id-name data-model-id= (if (typep object 'object-with-oid) (oid object) (store-object-id object)) >
             <label class="custom-control-label" for=id-name />
             ,(car children)
         </div> <!-- end checkbox -->
       </div> <!-- end col -->
       <div class="col-sm-6">
         <div class="d-flex justify-content-between">
           <div class= "mb-1 mt-1" >
             ,(cadr children)
           </div>
           <div class= "mt-1" >
             ,(caddr children)
           </div>
         </div> <!-- end .d-flex-->
       </div> <!-- end col -->
     </div>)))

(deftag timeago (&key timestamp)
  (let* ((timestamp
           (cond
             ((numberp timestamp)
              (local-time:universal-to-timestamp timestamp))
             (t
              timestamp)))
         (timestamp (format nil "~a" timestamp)))
   (let ((timestamp (format nil "~a" timestamp)))
     <:time class= "timeago" datetime= timestamp >
       ,(progn timestamp)
     </:time>)))

(deftag taskie-timestamp (&key prefix timestamp)

  <ul class="list-inline font-13 text-end">
    <li class="list-inline-item">
      <mdi name= "today" />
      ,(progn prefix)
      <timeago timestamp=timestamp />
    </li>
  </ul>)

(defun %with-pagination (data body &key prev)
  (let ((n 50))
    (let* ((next-page-data
             (nthcdr n data))
           (this (nibble ()
                   (%with-pagination data body
                                     :prev prev)))
           (next (when next-page-data
                   (nibble ()
                     (%with-pagination next-page-data body
                                       :prev this)))))
      (funcall body (first-n n data) next prev))))

(defmacro with-pagination ((page data &key (next-link (gensym "NEXT-LINK"))
                                        (prev-link (gensym "PREV-LINK")))
                           &body body)
  `(flet ((body (,page ,next-link ,prev-link) ,@body))
     (%with-pagination ,data #'body)))
