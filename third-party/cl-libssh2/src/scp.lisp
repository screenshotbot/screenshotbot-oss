;; -*- mode: lisp; syntax: common-lisp -*-

(in-package :libssh2)

(defun scp-get (remote-name local-name &optional (connection *ssh-connection*))
  (with-scp-input (in connection remote-name stat)
    (with-open-file (out local-name
                         :direction :output
                         :if-exists :supersede
                         :if-does-not-exist :create
                         :element-type '(unsigned-byte 8))
      (cl-fad:copy-stream in out))))

(defun scp-put (local-name remote-name &optional (connection *ssh-connection*))
  (with-open-file (in local-name
                      :direction :input
                      :element-type '(unsigned-byte 8))
    (with-scp-output (out connection remote-name
                      (file-length in))
      (cl-fad:copy-stream in out))))
