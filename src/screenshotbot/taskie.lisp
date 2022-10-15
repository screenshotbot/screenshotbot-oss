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
  (:import-from #:local-time
                #:format-timestring)
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
  <div class= "page-title-box main-content">
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
                     (checkboxes t)
                     next-link
                     prev-link)
  <markup:merge-tag>
  <table class= (format nil "main-content taskie-list mt-3 mb-3 ~a ~a"
                      (if items
                          "nonempty"
                          "empty")
                                  (if checkboxes "checkboxes")) >

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
             id=id-name data-model-id= (if (typep object 'object-with-oid) (ignore-errors (oid object)) (store-object-id object)) >
        </div> <!-- end checkbox -->

      </td>)

      ,@ (loop for child in children
               collect <td>,(progn child)</td>)
    </tr>))

(defvar *ts-format-cache* (make-hash-table))

(defvar *current-year*
  (local-time:timestamp-year (local-time:now)))

(defun format-ts (timestamp)
  (let ((key (cond
               ((numberp timestamp)
                timestamp)
               (t
                (local-time:timestamp-to-universal timestamp)))))
   (util:or-setf
    (gethash key *ts-format-cache*)
    (format nil
            (format nil "~a"
                    (local-time:universal-to-timestamp key))))))

(defun human-render-local-time (local-time)
  (cond
    ((eql *current-year*
          (local-time:timestamp-year local-time))
     (format-timestring nil local-time
                        :format '(:long-month " " :day)))
    (t
     (format-timestring nil local-time
                        :format '(:long-month " " :day " " :year)))))

(deftag timeago (&key timestamp)
  (multiple-value-bind (key local-time)
      (cond
        ((numberp timestamp)
         (values timestamp (local-time:universal-to-timestamp timestamp)))
        (t
         (values (local-time:timestamp-to-universal timestamp) timestamp)))
    (let* ((timestamp (format-ts key)))
      <:time class= (when (> key (- (get-universal-time) (* 30 24 3600))) "timeago") datetime= timestamp title= (format-timestring nil local-time :format local-time:+rfc-1123-format+) >
      ,(human-render-local-time local-time)
      </:time>)))

(deftag taskie-timestamp (&key prefix timestamp)

    <span class= "taskie-timestamp" >
      <mdi name= "today" />
      ,(progn prefix)
      <timeago timestamp=timestamp />
    </span>)

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
