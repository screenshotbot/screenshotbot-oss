(defpackage :screenshotbot/dashboard/explain
  (:use #:cl)
  (:import-from #:markup
                #:deftag)
  (:import-from #:screenshotbot/template
                #:mdi)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:explain))
(in-package :screenshotbot/dashboard/explain)

(markup:enable-reader)

(deftag explain (explanation &key title)
  <sup><a data-bs-toggle= "popover" title= title
          class= "explain-icon"
           data-bs-trigger= "focus"
           data-bs-html= "true"
           data-bs-content= (markup:write-html <div>,@ (progn explanation)</div>) href= "#" ><mdi name= "help" /></a></sup>)
