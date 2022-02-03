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

(defun paginated (components &optional (num 24))
  (let* ((this-page (util/lists:head components num))
         (rest (util/lists:tail components num))
         (load-more (nibble ()
                      (paginated rest))))
    <div class= "row pb-4 load-more-container" >
      ,@this-page

      ,(when rest
      <div class= "col-12 d-flex justify-content-center">
        <button class= "btn btn-primary load-more-button" data-load-more=load-more >Load More</button>
      </div>)
  </div>))
