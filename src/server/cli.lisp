(defpackage :server/cli
  (:use #:cl)
  (:import-from #:clingon
                #:make-option)
  (:export
   #:main))
(in-package :server/cli)

(defun run/command ()
  (clingon:make-command :name "run"
                        :options (list)))

(defun main/handler (cmd)
  (clingon:print-usage-and-exit cmd t))

(defun main/command ()
  (clingon:make-command :name "App Server"
                        :handler #'main/handler
                        :sub-commands nil))

(defun legacy-mode-p (args)
  (and (second args)
       (eql #\- (elt (second args) 0))))

(defun main (&key jvm acceptor enable-store)
  (cond
    ((legacy-mode-p sys:*line-arguments-list*)
     (warn "Using legacy mode for command line parsing")
     (server:main :jvm jvm :acceptor acceptor :enable-store enable-store))
    (t
     (let ((args #-lispworks (cdr (uiop:raw-command-line-arguments))
                 #+lispworks (cdr sys:*line-arguments-list*)))
       (let ((app (main/command)))
         (clingon:run app args))))))
