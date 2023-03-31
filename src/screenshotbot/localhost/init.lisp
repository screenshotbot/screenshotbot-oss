(defpackage :screenshotbot/localhost/init
  (:use #:cl)
  (:export
   #:main))
(in-package :screenshotbot/localhost/init)

(defun main ()
  (format t "hello world~%"))
