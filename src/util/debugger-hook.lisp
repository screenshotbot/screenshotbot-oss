(defpackage :util/debugger-hook
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:log4cl-debugger-hook))
(in-package :util/debugger-hook)

(defun log4cl-debugger-hook (condition debugger-hook)
  (declare (ignore debugger-hook))
  (handler-case
      (let ((output (with-output-to-string (out)
                      (trivial-backtrace:print-condition
                       condition
                       out)
                      #+lispworks
                      (dbg:output-backtrace
                       :brief
                       :stream out))))
        (log:error "~a" output))
    (error (e)
      (format t "FATAL: log4cl-debugger-hook failed so we can't log crashes, DIE DIE DIE! Please report to arnold@screenshotbot.io: ~a" e)
      (uiop:quit 1))))

#+nil
(log4cl-debugger-hook (make-condition 'error)
                      #'log4cl-debugger-hook)
