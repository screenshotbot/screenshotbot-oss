(defpackage :screenshotbot-js-stubs
  (:use #:cl
        #:ps)
  (:export #:make-matrix))

(defpackage :screenshotbot-js
  (:use #:cl
        #:ps
        #:screenshotbot-js-stubs)
  (:import-from #:3d-vectors
                #:v*
                #:vec3
                #:vx3
                #:vec
                #:v-)
  (:import-from #:3d-matrices
                #:m+
                #:m*))

(setf (ps:ps-package-prefix :3d-matrices) "_mat_")
(setf (ps:ps-package-prefix :3d-vectors) "_vec_")
