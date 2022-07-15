(defpackage :scale/vagrant
  (:use #:cl
        #:scale/core)
  (:local-nicknames (#:a #:alexandria)))
(in-package :scale/vagrant)

(defclass instance ()
  ((directory :initarg :directory
              :reader tmpdir)))

(defclass vagrant ()
  ())


(defun run* (cmd &key (output *standard-output*)
                   (error-output *standard-output*)
                   directory)
  (uiop:run-program
   cmd
   :output output
   :error-output error-output
   :directory directory))

(defmethod create-instance ((self vagrant) type &key &allow-other-keys)
  (declare (ignore type))
  (log:info "Starting container")
  (let ((directory (tmpdir:mkdtemp)))
    (with-open-file (out (path:catfile directory "Vagrantfile")  :direction :output)
      (write-string
       "Vagrant.configure('2') do |config|
  config.vm.box = \"debian/bullseye64\"
end" out))
    (run*
     (list "vagrant" "up")
     :directory directory)
    (make-instance 'instance
                    :directory directory)))

(defmethod delete-instance ((self instance))
  (log:info "Stopping container")
  (run*
   (list "vagrant" "halt")
   :directory (tmpdir self))
  (log:info "Destroying container")
  (run*
   (list "vagrant" "destroy" "-f" "--no-tty")
   :directory (tmpdir self))
  (fad:delete-directory-and-files (tmpdir self)))

(defmethod wait-for-ready ((self instance))
  t)

#+nil
(with-instance (instance (make-instance 'vagrant) :small)
  nil)
