;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/dashboard/bisect
  (:use #:cl)
  (:import-from #:screenshotbot/template
                #:app-template)
  (:import-from #:screenshotbot/dashboard/compare
                #:link-to-run
                #:screenshot-box)
  (:import-from #:screenshotbot/user-api
                #:recorder-run-commit)
  (:import-from #:nibble
                #:nibble)
  (:import-from #:markup
                #:deftag)
  (:import-from #:core/ui/simple-card-page
                #:simple-card-page)
  (:export
   #:bisect-item))
(in-package :screenshotbot/dashboard/bisect)

(named-readtables:in-readtable markup:syntax)

(defclass bisect-item ()
  ((screenshot :initarg :screenshot
               :reader item-screenshot)
   (run :initarg :run
        :reader item-run)))

(defclass state ()
  ((items :initarg :items
          :reader items
          :documentation "An fset sequence of items that are currently being bisected.")))

(defun midpoint-pos (state)
  (floor (fset:size (items state)) 2))

(defun midpoint (state)
  (let ((pos (midpoint-pos state)))
    (values (fset:lookup (items state) pos) pos)))

(defun state-if-good (state)
  "Returns a state if the midpoint is good"
  (make-instance 'state
                 :items (fset:subseq (items state)
                                     0 (1+ (midpoint-pos state)))))

(defun state-if-bad (state)
  (make-instance 'state
                 :items (fset:subseq (items state)
                                     (midpoint-pos state)
                                     (fset:size (items state)))))

(defmethod render-bisection (state)
  (let ((num-items (fset:size (items state))))
    (assert (>= num-items 2))
    (cond
      ((= (fset:size (items state)) 2)
       ;; We got our result
       (render-result (fset:lookup (items state) 0)))
      (t
       (flet ((bisect-nibble (fn)
                (nibble ()
                  (render-bisection
                   (funcall fn state)))))
         (let ((midpoint (midpoint state))
                (pos (midpoint-pos state)))
            (assert (> pos 0))
            (assert midpoint)
           <simple-card-page>
             <div class= "card-header">
               <h3>Bisecting</h3>
               <p class= "text-muted mb-1">
                 Currently looking at <link-to-run run= (item-run midpoint) />
               </p>
             </div>
             <div  class= "card-body" >
               <screenshot-box screenshot= (item-screenshot midpoint) />
             </div>

             <div class="card-footer">
               <div class= "text-muted mb-2">
                 Is this image good or bad?
               </div>
               <a href= (bisect-nibble #'state-if-good) class= "btn btn-success" >Good</a>
               <a href= (bisect-nibble #'state-if-bad) class= "btn btn-danger" >Bad</a>
             </div>
           </simple-card-page>))))))

(defun render-result (item)
  <simple-card-page>
    <div>
      The first bad run is: <link-to-run run= (item-run item) />
    </div>
  </simple-card-page>)

(defun bisect-page (items)
  (render-bisection
   (make-instance 'state
                  :items (fset:convert 'fset:seq items))))
