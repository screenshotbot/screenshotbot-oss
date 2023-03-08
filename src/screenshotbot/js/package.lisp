(defpackage :screenshotbot-js-stubs
  (:use #:cl
        #:ps)
  (:export #:make-matrix))

(defpackage :screenshotbot-js
  (:use #:cl
        #:ps
        #:screenshotbot-js-stubs))
