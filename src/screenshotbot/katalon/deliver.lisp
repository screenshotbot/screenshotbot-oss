(defpackage :screenshotbot/katalon/deliver
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria)))
(in-package :screenshotbot/katalon/deliver)

(defvar *init-hooks* nil)

(defun init-fn ()
  (format t "~%~%~%;;; Init screenshotbot library~%~%~%")
  (mapc #'funcall *init-hooks*))

(defun deliver-lib (&key debug output)
  (when debug
    (ql:quickload :slynk))
  (push (lambda ()
          (uiop:symbol-call :slynk :create-server :port 4009))
        *init-hooks*)
  (lw-ji:setup-deliver-dynamic-library-for-java
   :init-java nil)
  (lw:deliver 'init-fn
           output
           (if debug 0 5)
           :image-type :dll))

(defun safe-prin (x)
  (let ((*package* (find-package :cl-user)))
    (prin1-to-string x)))

(defun build (&key debug)
  (let ((output (asdf:system-relative-pathname :screenshotbot.katalon "katalon-plugin.so")))
   (uiop:run-program
    (list
     (lw:lisp-image-name)
     "-eval"
     (safe-prin
      `(progn
         (ql:quickload :screenshotbot.katalon)))
     "-eval"
     (safe-prin
      `(progn
         (deliver-lib :debug ,debug
                      :output ,output))))
    :output *standard-output*
    :error-output *standard-output*)
    (log:info "Delivered file size: ~aMB" (floor (trivial-file-size:file-size-in-octets output) (* 1024 1024)))))


;; (build :debug t)
