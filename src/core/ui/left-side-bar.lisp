;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :core/ui/left-side-bar
  (:use #:cl)
  (:import-from #:markup
                #:deftag)
  (:import-from #:core/ui/mdi
                #:mdi))
(in-package :core/ui/left-side-bar)

(named-readtables:in-readtable markup:syntax)


(deftag left-nav-item (children &key href image-class target
                       (script-name (error "need script-name")))
  (declare (ignore target))
  (let ((activep (str:starts-with-p href script-name)))
    <li class= "nav-item"  >
      <a href= href class= (format nil "nav-link ~a text-white" (if activep "active" "")) >
        <mdi name=image-class />
        <span class= "text">,@children </span>
      </a>
    </li>))
