(defpackage :scale/vagrant
  (:use #:cl
        #:scale/core)
  (:local-nicknames (#:a #:alexandria)))
(in-package :scale/vagrant)

(defclass instance (base-instance)
  ((directory :initarg :directory
              :reader tmpdir)
   (ip-address :accessor ip-address)
   (known-hosts :accessor known-hosts)
   (ssh-port :accessor ssh-port)))

(defmethod ssh-user ((self instance))
  "vagrant")

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
  config.vm.provider \"virtualbox\" do |v|
    v.memory = 2048
  end
end" out))
    (run*
     (list "vagrant" "up")
     :directory directory)
    (let ((ret (make-instance 'instance
                              :directory directory)))
      (handler-bind ((error (lambda (e)
                              (declare (ignore e))
                              (log:info "Couldn't finish pre-setup for instance")
                              (delete-instance ret))))
        (prepare-vagrant-instance ret directory))
      ret)))

(auto-restart:with-auto-restart ()
  (defun prepare-vagrant-instance (ret directory)
    (run*
     (list "vagrant" "scp" (namestring (secret-file "id_rsa.pub"))
          "default:/home/vagrant/id_rsa.pub")
     :directory directory)

    (ssh-run-via-vagrant ret
                         "cat id_rsa.pub >> ~vagrant/.ssh/authorized_keys && cat ~vagrant/.ssh/authorized_keys")
   (let ((known-hosts (ssh-run-via-vagrant ret
                                           "ssh-keyscan localhost"
                                           :output 'string)))
     (log:info "Got known hosts: ~a" known-hosts)
     (setf (known-hosts ret) known-hosts))
   (let ((ssh-config (run* (list "vagrant" "ssh-config")
                           :directory directory
                           :output 'string)))
     (flet ((read-config (name)
              (loop for line in (str:lines ssh-config)
                    for (key val) = (str:split " " (str:trim line))
                    if (string-equal key name)
                      return val)))

       (setf (ip-address ret) (read-config "HostName"))
       (setf (ssh-port ret) (parse-integer (read-config "Port")))))))

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

(defmethod ssh-run-via-vagrant ((self instance) (cmd string)
                                &key (output *standard-output*)
                                  (error-output *error-output*))
  (log:info "Running command ~S on vagrant" cmd)
  (run*
   (list "vagrant" "ssh" "-c" (format nil "sudo bash -c ~a" (uiop:escape-sh-token cmd)))
   :output output
   :error-output error-output
   :directory (tmpdir self)))


#+nil
(with-instance (instance (make-instance 'vagrant) :small)
  (ssh-run instance "whoami")
  (scp instance "~/default.css" "/root/default.css"))
