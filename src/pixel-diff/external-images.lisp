(defpackage :pixel-diff/external-images
  (:use #:cl
        #:capi)
  (:import-from #:easy-macros
                #:def-easy-macro)
  (:export
   #:with-image-from-external))
(in-package :pixel-diff/external-images)

(def-easy-macro with-image-from-external (&binding image pane external-image &fn fn)
  (let ((image (gp:load-image pane external-image :editable t)))
    (unwind-protect
         (fn image)
      (gp:free-image pane image))))