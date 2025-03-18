;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/api/doc
  (:use #:cl)
  (:import-from #:alexandria
                #:assoc-value)
  (:import-from #:screenshotbot/server
                #:staging-p)
  (:export
   #:def-api-doc
   #:render-all-api-docs))
(in-package :screenshotbot/api/doc)

(named-readtables:in-readtable markup:syntax)

(defvar *docs* nil)

(defclass api-doc ()
  ((endpoint :initarg :endpoint
             :reader endpoint)
   (input-type :initarg :input-type
               :reader input-type)
   (output-type :initarg :output-type
                :reader output-type)
   (title :initarg :title
          :reader title)
   (method :initarg :method
           :reader api-method)
   (generator :initarg :generator
              :reader generator
              :documentation "A function that generates the given documentation")))

(easy-macros:def-easy-macro def-api-doc (endpoint &fn fn
                                                  &rest args)
  (setf (assoc-value *docs* endpoint :test #'equal)
        (apply #'make-instance
               'api-doc
               :endpoint endpoint
               :generator #'fn
               args)))

(markup:deftag render-all-api-docs ()
  (cond
   ((staging-p)
     <div>
     ,@(loop for (nil . doc) in *docs*
             collect
             <div>
             <h2 id= (title doc)>,(title doc)</h2>

             <p>
             <span class= "text-success">,(string (api-method doc))</span>
             ,(endpoint doc)
             </p>
             ,(funcall (generator doc))
             </div>)
     </div>)
   (t
    <div />)))

