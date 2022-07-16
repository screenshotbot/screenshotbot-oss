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

(defmethod ssh-run ((self instance)
                    (cmd string)
                    &key (output *standard-output*) (error-output *standard-output*))
  (log:info "Running command ~S on vagrant" cmd)
  (run*
   (list "vagrant" "ssh" "-c" (format nil "sudo bash -c ~a" (uiop:escape-sh-token cmd)))
   :output output
   :error-output error-output
   :directory (tmpdir self)))

(auto-restart:with-auto-restart ()
 (defmethod scp ((self instance)
                 from
                 to)
   (log:info "Copying file")
   (let ((tmpfile (format nil "/tmp/a~a" (secure-random:number 10000000000000000000))))
     (run*
      (list "vagrant" "scp"
            (namestring (truename from)) (format nil ":~a" tmpfile))
      :directory (tmpdir self))
     (ssh-run
      self
      (list "mv" tmpfile (namestring to)))
     (ssh-run
      self
      (list "chown" "root:root" (namestring to))))))

#+nil
(with-instance (instance (make-instance 'vagrant) :small)
  (ssh-run instance "whoami")
  (scp instance "~/default.css" "/root/default.css"))
