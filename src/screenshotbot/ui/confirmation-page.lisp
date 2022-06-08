;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(uiop:define-package :screenshotbot/ui/confirmation-page
  (:use #:cl #:alexandria)
  (:export #:confirmation-page
           #:confirmation-modal)
  (:import-from #:screenshotbot/ui/simple-card-page
                #:simple-card-page))
(in-package :screenshotbot/ui/confirmation-page)

(markup:enable-reader)

(markup:deftag confirmation-page (children &key  yes no)
  <simple-card-page>
    <p>,@(progn children)</p>
    <div class= "card-footer">
      <a href= yes class="btn btn-primary" >Yes</a>
      <a href= no class= "btn btn-secondary">No</a>
    </div>
  </simple-card-page>)

(markup:deftag confirmation-modal (children &key yes title)
  (let ((id (format nil "a-~a" (random 1000000))))
    <div class= "modal fade" id=id tabindex= "-1" role= "dialog">
      <div class= "modal-dialog" role= "dialog">
        <div class= "modal-content">

          ,(when title
          <div class= "modal-header">
            <strong>,(progn title)</strong>
          </div>)

    <div class= "modal-body">
      ,@children
    </div>

    <div class= "modal-footer">
      <form action=yes method= "post">
        <input type= "submit" value= "Yes" class= "btn btn-primary" />
      </form>
      <input type= "button" class= "btn btn-secondary" value= "No"
             data-bs-dismiss= "modal"
             data-bs-target= (format nil "#~a" id) />
    </div>
        </div>
      </div>
    </div>))
