(uiop:define-package :util/random-port
  (:use #:cl
        #:alexandria)
  (:import-from #:easy-macros
                #:def-easy-macro)
  (:import-from #:util/misc
                #:make-mp-hash-table)
  (:export #:random-port
           #:with-random-port))
(in-package :util/random-port)

(defvar *ports* (make-mp-hash-table
                 #+lispworks #+lispworks
                 :weak-kind :value))

(defvar *lock* (bt:make-lock))

(defun random-port (&key (referenced-from
                          ;; Agressively garbage collect by default
                          (make-instance 'standard-object)))
  (flet ((get-port ()
           (let ((socket (usocket:socket-listen "127.0.0.1" 0)))
             (unwind-protect
                  (usocket:get-local-port socket)
               (usocket:socket-close socket)))))
    (loop for x = (get-port)
          if (and
              (> x 20000)
              (ensure-port-set x referenced-from))
            return x)))

(defun ensure-port-set (port referenced-from)
  "Sets the port in *ports*, if it's already set return nil"
  (bt:with-lock-held (*lock*)
    (cond
      ((gethash port *ports*)
       nil)
      (t
       (setf (gethash port *ports*) referenced-from)
       t))))

(def-easy-macro with-random-port (&binding port &fn fn)
  "A version of random-port, while ensuring that no two threads get the
same port within the current application."
  (let ((port (random-port :referenced-from :never-remove)))
    (unwind-protect
         (fn port)
      (remhash port *ports*))))
