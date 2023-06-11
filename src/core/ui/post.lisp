;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :core/ui/post
  (:use #:cl)
  (:import-from #:markup
                #:deftag)
  (:import-from #:parenscript
                #:ps)
  (:export
   #:post-a))
(in-package :core/ui/post)

(named-readtables:in-readtable markup:syntax)

(deftag post-a (children &key href class action title)
  (let ((form-id (format nil "post-a-~a" (random 10000000000000000000))))
    <form action=action method= "POST" id=form-id class= "d-inline-block" >
      <a href=href class=class title=title
         onclick= (ps (submit (get-parent-element this))) >,@ (progn children)</a>
    </form>))
