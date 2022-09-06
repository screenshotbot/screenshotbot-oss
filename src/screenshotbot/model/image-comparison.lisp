(defpackage :screenshotbot/model/image-comparison
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria)))
(in-package :screenshotbot/model/image-comparison)

(let ((sym (ignore-errors
            (uiop:find-symbol* :image-comparison
                               :screenshotbot/dashboard/compare))))
  (when sym
   (import sym)
   (unintern sym :screenshotbot/dashboard/compare)
   (import sym)))


(uiop:export* :image-comparison *package*)
