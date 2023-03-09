(in-package :screenshotbot-js)

;; These are all utility functions for canvas, in particular
;; loadIntoCanvas, for testability reasons.

(defparameter *min-zoom* 0.2
  "min-zoom is interesting. Let's call this z_0 for now.

It's not the minimum zoom level. Instead, it's saying that for a given dimention (width or height), it's saying that the number of pixels the rendered image took divided by the client dimenion is greater than z_0.

Let's do some math. If M is the final transformation matrix:

|| M[w 0 0] - M[0 0 0] || >= z_o * w_c

Where w_c is the client width, and w is the image width.

This gives us
|| M[w 0 0] || >= z_0 *w_c

or, simply:
z*w >= z_0 * w_c

So we know, z >= z_0 * w_c / w.
")

(defmacro with-css-zoom-calcs (&body body)
  `(flet ((zor (x y)
            (if (= x 0)
                y
                x)))
     (let* ((client-width (zor client-width width))
            (client-height (zor client-height height)))
       ,@body)))

(defun calc-core-transform (client-width
                            client-height
                            width
                            height)
  (with-css-zoom-calcs
    (let* ((w-ratio (/ client-width width))
           (h-ratio (/ client-height height))
           (z (max
               (min w-ratio
                    h-ratio)
               ;; See documentation for *min-zoom*
               (* w-ratio *min-zoom*)
               (* h-ratio *min-zoom*))))
      (flet ((calc-t (client-dim dim)
               (/ (- client-dim (* z dim)) 2)))
        (let ((tx (calc-t client-width width))
              (ty (calc-t client-height height)))
          (make-matrix
           z 0 0 z (max tx 0) (max ty 0)))))))

(defun calc-transform-for-center (client-width
                                  client-height
                                  width
                                  height
                                  x y
                                  zoom)
  "Given an x,y position, figure out the translation matrix such that x,y
will be in the center of the screen and zoomed in at level z.

We know p=(x,y) should end up being at d=(client-width/2, client-height/2).

d = MCp

Where M is the final transform that we want to solve for. If we break
up M as a zoom matrix and a translation matrix we get:

d = (Z + T)Cp

We know that Z is just zoom*I.

so we get:

d = (zI + T)Cp
or
TCp = d - zCp.

Now notice, that T looks like:

[0 0 tx
 0 0 ty
 0 0 0]

Cx is a vector like [x' y' c]. So TCp just evaluates to t = [tx ty c], for some constants c,
irrespective of what's in Cp.

We can thus write:

t = d - zCp.

Once we have t, we can construct our final matrix.
"

  (with-css-zoom-calcs
    (let* ((C (calc-core-transform client-width
                                   client-height
                                   width
                                   height))
           (d (3d-vectors:vec3 (/ client-width 2)
                               (/ client-height 2)
                               0))
          (tt (v- d
                  (v* zoom (m* C (3d-vectors:vec3 x y 0))))))
     (make-matrix
      zoom 0
      0 zoom
      (3d-vectors:vx3 tt) (3d-vectors:vy3 tt)))))

(defun animate-transform (old-transform new-transform progress)
  (m+
   (m* (- 1 progress)
       old-transform)
   (m* progress
       new-transform)))
