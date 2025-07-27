(in-package :screenshotbot-js-stubs)


(defun make-matrix-impl (a b c d e f)
  (3d-matrices:mat
   a c e
   b d f
   0 0 1))

(defvar *make-matrix-impl* #'make-matrix-impl)

(defun make-matrix (&rest args)
  (apply *make-matrix-impl* args))

(make-matrix 1 1 1 1 1 1 )
