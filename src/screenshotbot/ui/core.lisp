(pkg:define-package :screenshotbot/ui/core
    (:use #:cl
          #:alexandria)
  (:import-from #:markup
                #:deftag)
  (:export #:ui/a
           #:ui/div
           #:ui/span))

(markup:enable-reader)

(deftag ui/a (children &key href class btn id)
  (let* ((class (or
                 (when btn
                   (format nil "btn btn-~a ~a" (string-downcase btn) class))
                 class)))
    <a href=href class=class id=id >,@(progn children)</a> ))

(deftag ui/div (children &key class)
  <div class=class >,@ (progn children)</div>)

(deftag ui/span (children &key class)
  <span class=class >,@ (progn children)</span>)
