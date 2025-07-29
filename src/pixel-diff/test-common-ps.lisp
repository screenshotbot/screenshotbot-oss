(defpackage :screenshotbot/js/test-common-ps
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot-js
                #:calc-transform-for-zoom
                #:calc-transform-for-center
                #:*min-zoom*
                #:calc-core-transform)
  (:import-from #:3d-matrices
                #:mat
                #:m*
                #:m=)
  (:import-from #:3d-vectors
                #:v=
                #:vec))
(in-package :screenshotbot/js/test-common-ps)

(util/fiveam:def-suite)

(defvar *id* (mat
              1 0 0
              0 1 0
              0 0 1))

(test calc-core-transform
  (is
   (m=
    *id*
    (calc-core-transform 0 0 10 20))))

(test calc-core-transform-with-same-dims
  (is
   (m=
    *id*
    (calc-core-transform 10 20 10 20))))

(test calc-core-transform-with-double-dims
  (is
   (m=
    (mat
     2 0 0
     0 2 0
     0 0 1)
    (calc-core-transform 20 40 10 20))))

(test calc-core-transform-with-not-enough-height
  (is
   (m=
    (mat
     1 0 5 ;; translate the x by 5
     0 1 0
     0 0 1)
    (calc-core-transform 20 20 10 20))))

(test calc-core-transform-with-not-enough-height-and-also-zoom
  (is
   (m=
    (mat
     1/2 0 5 ;; translate the x by 5
     0 1/2 0
     0 0 1)
    (calc-core-transform 20 20 20 40))))

(test calc-core-transform-with-not-enough-width
  (is
   (m=
    (mat
     1 0 0
     0 1 5
     0 0 1)
    ;; css zoom will be 1
    (calc-core-transform 20 20 20 10))))

(test calc-core-transform-with-not-enough-height-2
  (let ((*min-zoom* 0.5))
   (is
    (m=
     (mat
      1 0 5 ;; translate the x by 5
      0 1 0
      0 0 1)
     (calc-core-transform 20 20 10 2000)))))

(test calc-core-transform-with-not-enough-height-with-different-client-width
  (let ((*min-zoom* 0.5))
   (is
    (m=
     (mat
      0.1 0 5 ;; translate the x by 5
      0 0.1 0
      0 0 1)
     (calc-core-transform 20 20 100 20000)))))


(test calc-transform-for-center-simple
  (is
   (m=
    (mat
     1 0 0
     0 1 0
     0 0 1)
    (calc-transform-for-center 10 20
                               10 20
                               5 10
                               1))))

(test calc-transform-for-center-with-some-zoom
  (is
   (m=
    (mat
     2 0 -5
     0 2 -10
     0 0 1)
    (calc-transform-for-center 10 20
                               10 20
                               5 10
                               2))))

(test calc-transform-for-center-with-a-core-transform
  (let ((transform
          (calc-transform-for-center 800 500
                                     360 360
                                     0 0
                                     1))
        (core
          (calc-core-transform 800 500
                               360 360)))

    ;; Now 0,0 should be mapped to the center
    (log:info "Transform: ~a" transform)
    (log:info "Core: ~a" core)
    (is
     (v=
      (vec 400 250 1)
      (m* transform core (vec 0 0 1))))))

(test calc-transform-for-zoom-happy-path
  (is
   (m=
    (mat
     2 0 0
     0 2 0
     0 0 1)
    (calc-transform-for-zoom
     0 0
     (mat
      1 0 0
      0 1 0
      0 0 1)
     2))))

(defun m~= (a b &optional (tolerance 1e-3))
  (let ((diff (3d-matrices:m- a b)))
    (loop for i from 0 to 8
          always (< (abs (aref (3d-matrices::marr3 diff) i)) tolerance))))

(test calc-transform-for-zoom-invariant-after-multiple-zooms
  (let ((initial-transform (mat
                            1 0 0
                            0 1 0
                            0 0 1))
        (mouse-x 100)
        (mouse-y 150)
        (zoom-factor 1.5))
    (let ((transform-after-zooms initial-transform))
      ;; Zoom in 5 times
      (loop for i from 1 to 5 do
        (setf transform-after-zooms
              (m* (calc-transform-for-zoom mouse-x mouse-y
                                           transform-after-zooms
                                           zoom-factor)
                  transform-after-zooms)))
      ;; Zoom out 5 times
      (loop for i from 1 to 5 do
        (setf transform-after-zooms
              (m* (calc-transform-for-zoom mouse-x mouse-y
                                           transform-after-zooms
                                           (/ 1 zoom-factor))
                  transform-after-zooms)))
      ;; Should be back to identity matrix
      (is (m~= initial-transform transform-after-zooms)))))

(test calc-transform-for-zoom-invariant-after-multiple-zooms-with-initial-translation
  (let ((initial-transform (mat
                            1 0 -1000
                            0 1 -2000
                            0 0 1))
        (mouse-x 100)
        (mouse-y 150)
        (zoom-factor 1.5))
    (let ((transform-after-zooms initial-transform))
      ;; Zoom in 5 times
      (loop for i from 1 to 5 do
        (setf transform-after-zooms
              (m* (calc-transform-for-zoom mouse-x mouse-y
                                           transform-after-zooms
                                           zoom-factor)
                  transform-after-zooms)))
      ;; Zoom out 5 times
      (loop for i from 1 to 5 do
        (setf transform-after-zooms
              (m* (calc-transform-for-zoom mouse-x mouse-y
                                           transform-after-zooms
                                           (/ 1 zoom-factor))
                  transform-after-zooms)))
      ;; Should be back to identity matrix
      (is (m~= initial-transform transform-after-zooms)))))

(test calc-transform-for-zoom-invariant-after-multiple-zooms-back-and-forths
  (let ((initial-transform (mat
                            1 0 0
                            0 1 0
                            0 0 1))
        (mouse-x 100)
        (mouse-y 1500)
        (zoom-factor 1.5))
    (let ((transform-after-zooms initial-transform))
      (dotimes (i 3)
        ;; Zoom in 5 times
        (loop for i from 1 to 5 do
          (setf transform-after-zooms
                (m* (calc-transform-for-zoom mouse-x mouse-y
                                             transform-after-zooms
                                             zoom-factor)
                    transform-after-zooms)))
        ;; Zoom out 5 times
        (loop for i from 1 to 5 do
          (setf transform-after-zooms
                (m* (calc-transform-for-zoom mouse-x mouse-y
                                             transform-after-zooms
                                             (/ 1 zoom-factor))
                    transform-after-zooms))))
      ;; Should be back to identity matrix
      (is (m~= initial-transform transform-after-zooms 0.1)))))



