;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(uiop:define-package :core/ui/simple-card-page
  (:use #:cl #:alexandria)
  (:import-from #:core/ui/template
                #:app-template)
  (:export #:simple-card-page
           #:confirmation-page
           #:confirmation-modal))
(in-package :core/ui/simple-card-page)

(markup:enable-reader)

(markup:deftag simple-card-page (children &key (col-class "col-lg-4 col-md-8")
                                 title
                                 (max-width "30rem")
                                 script-name
                                 stripe
                                 form-action
                                 form-id)
  (let* ((children (remove-if 'stringp children))
         (footer (if (mquery:has-class-p (last children) "card-footer")
                     (car (last children))
                     nil))
         (children (if footer
                       (butlast children)
                       children))
         (header (if (mquery:has-class-p (first children) "card-header")
                     (car children)
                     nil))
         (children (if header
                       (cdr children)
                       children))
         (inner (progn
                  <div class= "card" >
                    ,(progn header)
                    <div class= "card-body">
                      ,@children
                    </div>
                    ,(progn footer)
                  </div>)))
    <app-template title=title stripe=stripe >
      <div class= "main-content" >
        <div class= "card-page-container mt-3 mx-auto"
             style= (when max-width
                     (format nil "max-width: ~a" max-width)) >
          ,(cond
             (form-action
              <form action=form-action id=form-id method= "POST" >
                ,(progn inner)
              </form>)
             (t
              inner))
        </div>
      </div>
    </app-template>))


(markup:deftag confirmation-page (children &key  yes no
                                  (danger nil))
  <simple-card-page>
    <p>,@(progn children)</p>
    <div class= "card-footer">
      <a href= yes class= (format nil "btn ~a" (if danger "btn-danger" "btn-primary")) >Yes</a>
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
