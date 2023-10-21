(defpackage :screenshotbot/showkase/main
  (:use #:cl)
  (:export
   #:main
   #:start-slynk))
(in-package :screenshotbot/showkase/main)

(defun start-slynk (port)
  (slynk:create-server :port port
                       :dont-close t)
  "success")

(defun sample ()
  "FOO")

(defun main ()
  (format t "Lispworks internal call SbInss"))
