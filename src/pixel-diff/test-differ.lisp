;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :pixel-diff/test-differ
  (:use #:cl
        #:fiveam)
  (:import-from #:pixel-diff/differ
                #:transform-to-3dmat
                #:3dmat-to-transform)
  (:import-from #:3d-matrices)
  (:import-from #:gp))
(in-package :pixel-diff/test-differ)

(in-suite :pixel-diff)

(test 3dmat-to-transform-and-inverse-happy-path-tests
  (let* ((test-cases '((1 0 0 1 0 0)              ; identity transform
                       (2 0 0 2 10 20)            ; scale and translate
                       (1.5 0 0 1.5 -5 15)        ; scale and translate with negatives
                       (0.5 0 0 0.5 100 -50))))    ; fractional scale
    (dolist (transform-data test-cases)
      (let* ((original-transform (apply #'gp:make-transform transform-data))
             (mat3 (transform-to-3dmat original-transform))
             (reconstructed-transform (3dmat-to-transform mat3)))
        (destructuring-bind (a1 b1 c1 d1 e1 f1) original-transform
          (destructuring-bind (a2 b2 c2 d2 e2 f2) reconstructed-transform
            (is (< (abs (- a1 a2)) 1e-10))
            (is (< (abs (- b1 b2)) 1e-10))
            (is (< (abs (- c1 c2)) 1e-10))
            (is (< (abs (- d1 d2)) 1e-10))
            (is (< (abs (- e1 e2)) 1e-10))
            (is (< (abs (- f1 f2)) 1e-10))))))))

(test 3dmat-to-transform-preserves-transformation-properties
  (let* ((original-transform (gp:make-transform 2 0 0 3 15 25))
         (mat3 (transform-to-3dmat original-transform))
         (reconstructed-transform (3dmat-to-transform mat3))
         (test-points '((0 0) (10 10) (-5 7) (100 -50))))
    (dolist (point test-points)
      (destructuring-bind (x y) point
        (multiple-value-bind (x1 y1)
            (gp:transform-point original-transform x y)
          (multiple-value-bind (x2 y2)
              (gp:transform-point reconstructed-transform x y)
            (is (< (abs (- x1 x2)) 1e-10))
            (is (< (abs (- y1 y2)) 1e-10))))))))

(test transform-to-3dmat-creates-valid-3x3-matrix
  (let* ((transform (gp:make-transform 1.5 0.2 -0.1 2.0 10 -15))
         (mat3 (transform-to-3dmat transform))
         (m (3d-matrices:marr mat3)))
    (destructuring-bind (a b c d e f) transform
      (is (= (aref m 0) a))
      (is (= (aref m 1) c))
      (is (= (aref m 2) e))
      (is (= (aref m 3) b))
      (is (= (aref m 4) d))
      (is (= (aref m 5) f))
      (is (= (aref m 6) 0))
      (is (= (aref m 7) 0))
      (is (= (aref m 8) 1)))))
