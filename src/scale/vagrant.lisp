(defpackage :scale/vagrant
  (:use #:cl
        #:scale/core)
  (:local-nicknames (#:a #:alexandria)))
(in-package :scale/vagrant)

(defvar *lock* (bt:make-lock))

(defclass instance (base-instance)
  ((directory :initarg :directory
              :reader tmpdir)
   (ip-address :accessor ip-address)
   (known-hosts :accessor known-hosts)
   (ssh-port :accessor ssh-port)
   (vagrant :initarg :vagrant
            :reader vagrant)
   (config :accessor instance-config
           :initarg :config)))

(defclass config ()
  ((name :initarg :name
         :reader config-name)
   (box :initarg :box
        :reader config-box)))

(defmethod ssh-user ((self instance))
  "vagrant")

(defvar *ctr* 0)

(defclass vagrant ()
  ((configs :initform nil
            :accessor vagrant-configs)))


(defun run* (cmd &key (output *standard-output*)
                   (error-output *standard-output*)
                   directory)
  (uiop:run-program
   cmd
   :output output
   :error-output error-output
   :directory directory))

(defun vagrant-dir ()
  (ensure-directories-exist
   (asdf:system-relative-pathname :screenshotbot "../../build/vagrant/")))

(defun write-configs (self)
  (with-open-file (out (path:catfile (vagrant-dir) "Vagrantfile")
                       :if-exists :supersede
                       :direction :output)
    (write-line
     "Vagrant.configure('2') do |config|" out)

    (loop for config in (vagrant-configs self)
          do
             (format out
                     "config.vm.define \"~a\" do |m|
  m.vm.box = \"~a\"
  m.vm.provider \"virtualbox\" do |v|
    v.memory = 2048
  end
end
  " (config-name config) (config-box config)))

    (write-line
           "end" out)))

(defmethod create-instance ((self vagrant) type &key &allow-other-keys)
  (declare (ignore type))
  (log:info "Starting container")
  (let* ((name (bt:with-lock-held (*lock*)
                 (format nil "machine-~a" (incf *ctr*))))
         (config (make-instance 'config
                                :name name
                                :box "debian/bullseye64")))
    (let ((directory (vagrant-dir)))
      (bt:with-lock-held (*lock*)
        (push config (vagrant-configs self))
        (write-configs self))
     (run*
      (list "vagrant" "up" name)
      :directory directory)
     (let ((ret (make-instance 'instance
                               :directory directory
                               :config config
                               :vagrant self)))
       (handler-bind ((error (lambda (e)
                               (declare (ignore e))
                               (log:info "Couldn't finish pre-setup for instance")
                               (delete-instance ret))))
         (prepare-vagrant-instance ret directory))
       ret))))

(auto-restart:with-auto-restart ()
  (defun prepare-vagrant-instance (ret directory)
    (run*
     (list "vagrant" "scp" (namestring (secret-file "id_rsa.pub"))
           (format nil "~a:/home/vagrant/id_rsa.pub"
                   (config-name (instance-config ret))))
     :directory directory)

    (ssh-run-via-vagrant ret
                         "cat id_rsa.pub >> ~vagrant/.ssh/authorized_keys && cat ~vagrant/.ssh/authorized_keys")
   (let ((known-hosts (ssh-run-via-vagrant ret
                                           "ssh-keyscan localhost"
                                           :output 'string)))
     (log:info "Got known hosts: ~a" known-hosts)
     (setf (known-hosts ret) known-hosts))
    (let ((ssh-config (run* (list "vagrant" "ssh-config"
                                  (config-name (instance-config ret)))
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
   (list "vagrant" "halt" (config-name (instance-config self)))
   :directory (tmpdir self))
  (log:info "Destroying container")
  (run*
   (list "vagrant" "destroy" "-f" "--no-tty" (config-name (instance-config self)))
   :directory (tmpdir self))
  (bt:with-lock-held (*lock*)
    (a:removef (vagrant-configs (vagrant self)) (instance-config self))
    (write-configs (vagrant self))))

(defmethod wait-for-ready ((self instance))
  t)

(defmethod ssh-run-via-vagrant ((self instance) (cmd string)
                                &key (output *standard-output*)
                                  (error-output *error-output*))
  (log:info "Running command ~S on vagrant" cmd)
  (run*
   (list "vagrant" "ssh"
         (config-name (instance-config self))
         "-c" (format nil "sudo bash -c ~a" (uiop:escape-sh-token cmd)))
   :output output
   :error-output error-output
   :directory (tmpdir self)))


#+nil
(with-instance (instance (make-instance 'vagrant) :small)
  (ssh-run instance "whoami")
  (scp instance "~/default.css" "/root/default.css"))
