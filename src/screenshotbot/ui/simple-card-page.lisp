(pkg:define-package :screenshotbot/ui/simple-card-page
    (:use #:cl
          #:alexandria)
  (:import-from #:../template
                #:app-template)
  (:export #:simple-card-page))

(markup:enable-reader)

(markup:deftag simple-card-page (children &key (col-class "col-lg-4 col-md-8")
                          form-action)
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
                  <div class= "card">
                    ,(progn header)
                    <div class= "card-body">
                      ,@children
                    </div>
                    ,(progn footer)
                  </div>)))
    <app-template>
      <div class= "row mt-3">
        <div class= col-class >
          ,(cond
             (form-action
              <form action=form-action method= "POST" >
                ,(progn inner)
              </form>)
             (t
              inner))
        </div>
      </div>

    </app-template>))
