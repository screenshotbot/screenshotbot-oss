(defpackage :screenshotbot/dashboard/explain
  (:use #:cl)
  (:import-from #:markup
                #:deftag)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:explain))
(in-package :screenshotbot/dashboard/explain)

(markup:enable-reader)

(deftag explain (explanation &key title)
  <sup>[<a data-bs-toggle= "popover" title= title
           data-bs-trigger= "focus"
           data-bs-html= "true"
           data-bs-content= (markup:write-html <div>,@ (progn explanation)</div>) href= "#" >?</a>]</sup>)
