;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :core/ui/taskie
  (:use :cl)
  (:import-from #:bknr.datastore
                #:store-object-id)
  (:import-from #:core/ui/mdi
                #:mdi)
  (:import-from #:markup/markup
                #:deftag
                #:unescaped)
  (:import-from #:nibble
                #:nibble)
  (:import-from #:util/lists
                #:head)
  (:import-from #:util/store/object-id
                #:object-with-oid
                #:oid)
  (:import-from #:util/timeago
                #:timeago)
  (:export
   #:taskie-list
   #:taskie-page-item
   #:taskie-page-title
   #:taskie-row
   #:taskie-timestamp
   #:timeago
   #:with-pagination))
(in-package :core/ui/taskie)

(markup:enable-reader)

(deftag taskie-page-title (children &key title class)
  <div class= (format nil "page-title-box main-content ~a" class) >
    <h4 class= "page-title" >,(progn title)
    </h4>

    <div class= "float-end">
      ,@children
    </div>
  </div>)

(deftag taskie-page-item (children &key href)
  <li class= (format nil "page-item ~a" (unless href "disabled"))>
    <a class= "page-link" href=href >
      ,@children
    </a>
  </li>)

(defvar *checkboxes*)

(deftag taskie-list (children &key empty-message items row-generator
                     headers
                     class
                     (checkboxes t)
                     next-link
                     prev-link)
  <markup:merge-tag>
  <table class= (format nil "main-content taskie-list mt-3 mb-3 ~a ~a ~a"
                      (if items
                          "nonempty"
                          "empty")
                      (if checkboxes "checkboxes")
                      class) >

      <thead>
        <tr>
          ,(when checkboxes
             <th>
             </th>)

          ,@ (loop for header in headers
                   collect <th>,(progn header)</th>)
        </tr>
      </thead>
      <tbody>

      ,@ (cond
           (items
            (loop for item in items collect
                  (let ((*checkboxes* checkboxes))
                   (funcall row-generator item))))
           (t
            (list
             <tr class= "empty" >
               <td colspan= "100%" class= "text-center pt-3 pb-3 text-muted" >
                 ,(progn empty-message)</td></tr>)))
      </tbody>

  </table>
  ,(when (or prev-link next-link)
       <nav aria-label="Page navigation" class= "mt-3" >
         <ul class="pagination justify-content-center">
           <taskie-page-item href=prev-link >Previous</taskie-page-item>
           <taskie-page-item href=next-link >Next</taskie-page-item>
  </ul>
</nav>
)
           </markup:merge-tag>)

(defvar *id-counter* 0)

(deftag taskie-row (children &key object)
  (let ((children (remove-if 'stringp children))
        (id-name (format nil "check-~a"(incf *id-counter*))))
    <tr>
      ,(when *checkboxes*
         <td>
           <div class="form-check">
             <input type="checkbox" class="recent-run-item form-check-input"
             id=id-name data-model-id= (if (typep object 'object-with-oid) (ignore-errors (oid object)) (store-object-id object)) />
        </div> <!-- end checkbox -->

      </td>)

      ,@ (loop for child in children
               collect <td>,(progn child)</td>)
    </tr>))

(deftag taskie-timestamp (&key prefix timestamp)

    <span class= "taskie-timestamp" >
      <mdi name= "today" />
      ,(progn prefix)
      <timeago timestamp=timestamp />
    </span>)

(defmethod head-and-tail (elements n)
  (head elements n))

(defmethod head-and-tail ((s fset:wb-set) n)
  "Our sorting order on datastore objects is by store object ids. This
means, that the newest elements are the greatest elements in our set."
  (labels ((build-page (s n so-far)
             (cond
               ((or (fset:empty? s) (= n 0))
                (values (nreverse so-far) s))
               (t
                (let ((next (fset:greatest s)))
                  (build-page
                   (fset:less s next)
                   (1- n)
                   (list* next so-far)))))))
    (build-page s n nil)))

(defun empty-data-p (next-page-data)
  "Either nil, or an empty set"
  (or
   (null next-page-data)
   (and
      (fset:set? next-page-data)
      (fset:empty? next-page-data))))

(defun %with-pagination (data body &key prev)
  (let ((n 50))
    (multiple-value-bind (this-page next-page-data)
        (head-and-tail data n)
     (let* ((this (nibble ()
                    (%with-pagination data body
                                      :prev prev)))
            (next (unless (empty-data-p next-page-data)
                    (nibble ()
                      (%with-pagination next-page-data body
                                        :prev this)))))
       (funcall body this-page next prev)))))

(defmacro with-pagination ((page data &key (next-link (gensym "NEXT-LINK"))
                                        (prev-link (gensym "PREV-LINK")))
                           &body body)
  `(flet ((body (,page ,next-link ,prev-link) ,@body))
     (%with-pagination ,data #'body)))
