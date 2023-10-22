(defpackage :screenshotbot/showkase/main
  (:use #:cl)
  (:import-from #:screenshotbot/showkase/lib
                #:*context*
                #:*target-context*)
  (:export
   #:main
   #:run
   #:start-slynk))
(in-package :screenshotbot/showkase/main)

(defun start-slynk (port)
  (slynk:create-server :port port
                       :dont-close t)
  "success")

(defun sample ()
  "FOO")

(defun run (context target-context)
  (setf *context* context)
  (setf *target-context* target-context)

  (slynk:create-server :port 4005
                       :dont-close t)
  (sleep 3000)
  (format t "Lispworks internal call SbInss")
  nil)

(defun main ()
  nil)
