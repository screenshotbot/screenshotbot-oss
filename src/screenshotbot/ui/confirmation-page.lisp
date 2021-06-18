(pkg:define-package :screenshotbot/ui/confirmation-page
    (:use :cl
     :alexandria)
  (:export #:confirmation-page)
  (:import-from ./simple-card-page
                #:simple-card-page))

(markup:enable-reader)

(markup:deftag confirmation-page (children &key  yes no)
  <simple-card-page>
    <p>,@(progn children)</p>
    <div class= "card-footer">
      <a href= yes class="btn btn-primary" >Yes</a>
      <a href= no class= "btn btn-secondary">No</a>
    </div>
  </simple-card-page>)
