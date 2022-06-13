(defpackage :screenshotbot/sdk/sdk-integration-tests
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria)))
(in-package :screenshotbot/sdk/sdk-integration-tests)

(ql:quickload :screenshotbot.sdk.deliver)

(defun run (cmd &rest args)
  (apply #'uiop:run-program
           (mapcar #'namestring cmd)
           (append
            args
            (list :output t
                  :error-output t))))

(defvar *sdk* (car (asdf:output-files 'asdf:compile-op
                                       (asdf:find-component :screenshotbot.sdk.deliver
                                                            "deliver-sdk"))))


(run (list *sdk* "--help"))

(assert (str:containsp "--static-website" (run (list *sdk* "--help") :output 'string)))
