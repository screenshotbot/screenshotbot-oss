(defpackage :screenshotbot/js/test-common-ps
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot-js
                #:calc-transform-for-center
                #:*min-zoom*
                #:calc-core-transform)
  (:import-from #:3d-matrices
                #:mat
                #:m*
                #:m=)
  (:import-from #:3d-vectors
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
      0.5 0 7.5 ;; translate the x by 5
      0 0.5 0
      0 0 1)
     (calc-core-transform 20 20 10 2000)))))

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
