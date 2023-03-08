(in-package :screenshotbot-js)

;; These are all utility functions for canvas, in particular
;; loadIntoCanvas, for testability reasons.

(defparameter *min-zoom* 0.1)

(defun calc-core-transform (client-width
                              client-height
                              width
                              height)
  (flet ((zor (x y)
           (if (= x 0)
               y
               x)))
    (let* ((client-width (zor client-width width))
           (client-height (zor client-height height))
           (w-ratio (/ client-width width))
           (h-ratio (/ client-height height)))
      (let ((css-zoom (max w-ratio h-ratio) #| object-fit: cover |#))
        (let ((z (max
                  (/ (min w-ratio
                          h-ratio)
                     css-zoom)
                  *min-zoom*)))
         (flet ((calc-t (client-dim dim)
                  (/ (/ (- client-dim (* z dim css-zoom)) 2)
                     css-zoom)))
           (let ((tx (calc-t client-width width))
                 (ty (calc-t client-height height)))
             (make-matrix
              z 0 0 z (max tx 0) (max ty 0)))))))))
