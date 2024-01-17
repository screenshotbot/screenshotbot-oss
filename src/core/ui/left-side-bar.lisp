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
                #:mdi)
  (:import-from #:core/ui/image
                #:img-with-fallback)
  (:export
   #:left-side-bar-container))
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

(deftag left-side-bar-container (children &key logo-small-src
                                 logo-src
                                 logo-alt)
    <div class="d-flex flex-column text-white bg-dark leftside-menu collapse" >

    <div class= "text-center p-3" >
      <button type= "button" href= "#" class="navbar-toggler" type="button" data-bs-toggle="collapse" data-bs-target=".leftside-menu" aria-controls="navbarSupportedContent" aria-expanded="false" aria-label="Toggle navigation">
        <mdi name= "menu" />
      </button>

      <a href="/runs" class="align-items-center mb-3 mb-md-0 me-md-auto text-white text-decoration-none">


        <span class="logo logo-lg fs-4">
          <img-with-fallback src= logo-src
                             alt= logo-alt
                             loading= "lazy" />
        </span>
        <span class="logo logo-sm fs-4">
          <img-with-fallback src= logo-small-src
                             alt= logo-alt
                             loading= "lazy" />
        </span>
      </a>
    </div>
  <hr class= "mt-0" />

  ,@children
  </div>)
