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
(defun main/command ()
  (clingon:make-command :name "App Server"
                        :subcommands (list (run/command)
                                           (verify/command)
                                           (self-test/command))))

(defun legacy-mode-p (args)
  (and (second args)
       (eql #\- (elt (second args) 0))))

(defun main (&key jvm acceptor enable-store)
  (cond
    ((legacy-mode-p sys:*line-arguments-list*)
     (warn "Using legacy mode for command line parsing")
     (server:main :jvm jvm :acceptor acceptor :enable-store enable-store))
    (t
     (error "unimpl"))))
