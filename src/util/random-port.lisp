(uiop:define-package :util/random-port
  (:use #:cl
        #:alexandria)
  (:import-from #:easy-macros
                #:def-easy-macro)
  (:import-from #:util/misc
                #:make-mp-hash-table)
  (:export #:random-port))
(in-package :util/random-port)

(defvar *ports* (make-mp-hash-table))
(defvar *lock* (bt:make-lock))

(defun random-port ()
  (flet ((get-port ()
           (let ((socket (usocket:socket-listen "127.0.0.1" 0)))
             (unwind-protect
                  (usocket:get-local-port socket)
               (usocket:socket-close socket)))))
    (loop for x = (get-port)
          if (> x 20000)
            return x)))

(defun ensure-port-set (port)
  "Sets the port in *ports*, if it's already set return nil"
  (bt:with-lock-held (*lock*)
    (cond
      ((gethash port *ports*)
       nil)
      (t
       (setf (gethash port *ports*) t)
       t))))

(def-easy-macro with-random-port (&binding port &fn fn)
  "A version of random-port, while ensuring that no two threads get the
same port within the current application."
  (block nil
    (loop
      (let ((port (random-port)))
        (when (ensure-port-set port)
          (unwind-protect
               (return-from nil (fn port))
            (remhash port *ports*)))))))
