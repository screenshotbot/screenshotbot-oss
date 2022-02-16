(defpackage :screenshotbot/image-comparison
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria)))
(in-package :screenshotbot/image-comparison)

(loop for sym in `(#:image-comparison
                   #:before
                   #:after
                   #:masks
                   #:identical-p
                   #:result)
      do
         (import (find-symbol (string sym) :screenshotbot/compare)))
