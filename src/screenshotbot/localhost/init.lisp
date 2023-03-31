(defpackage :screenshotbot/localhost/init
  (:use #:cl)
  (:export
   #:main))
(in-package :screenshotbot/localhost/init)

(defun main/handler (cmd)
  (clingon:print-usage-and-exit cmd t))

(defun main/command ()
  (clingon:make-command :name "Screenshotbot Local"
                        :version "0.1.0"
                        :description "Access Screenshotbots image comparison tools locally without internet access."
                        :authors (list "Modern Interpreters Inc")
                        :license "MPLv2"
                        :handler #'main/handler))
(defun main ()
  (uiop:setup-command-line-arguments)
  (let ((args (cdr (uiop:raw-command-line-arguments))))
    (let ((app (main/command)))
      (clingon:run app args))))
