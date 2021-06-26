(defpackage :util/random-port
  (:use #:cl
        #:alexandria)
  (:export #:random-port))
(in-package :util/random-port)

(defun random-port ()
  (flet ((get-port ()
           (let ((socket (usocket:socket-listen "127.0.0.1" 0)))
             (unwind-protect
                  (usocket:get-local-port socket)
               (usocket:socket-close socket)))))
    (loop for x = (get-port)
          if (> x 20000)
            return x)))
