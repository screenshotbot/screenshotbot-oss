;; -*- mode: lisp; syntax: common-lisp -*-

(in-package :libssh2)

(defvar *session* nil)

(defmethod method-of ((auth auth-password))  :PASSWORD)
(defmethod method-of ((auth auth-publickey)) :PUBLICKEY)
(defmethod method-of ((auth auth-agent))     :PUBLICKEY)

(defmethod authentication ((ssh ssh-connection) (auth-list list))
  (unless (or (null auth-list) (not (listp auth-list)))
    (let ((auth-methods (authentication-methods ssh (login (car auth-list))))
          (*errors-list* (remove :ERROR-AUTHENTICATION-FAILED *errors-list*)))
      (loop for auth in auth-list
         do (when (and (find (method-of auth)
                             auth-methods)
                       (authentication ssh auth))
              (return t))))))

(defun make-auth-data (login &key
                               (key-directories
                                (list (default-config-directory)))
                               (keys '(("id_rsa") ("id_dsa")))
                               (passwords '()))
  (unless (or (null keys)
              (not  (listp keys))
              (null (car keys))
              (not  (listp (car keys)))
              (null key-directories)
              (not  (listp key-directories)))
    (cons (make-agent-auth login)
          (apply #'concatenate
                 (append
                  (cons 'list
                        (loop for dir in key-directories
                           collect (loop for key in keys
                                      when (let ((path
                                                  (merge-pathnames (car key)
                                                                   dir)))
                                             (when (probe-file path)
                                               (make-publickey-auth
                                                login (namestring dir)
                                                (car key) (if (cdr key)
                                                              (cdr key) ""))))
                                      collect it)))
                  (list
                   (when (and passwords
                              (listp passwords))
                     (loop for password in passwords
                        collect (make-password-auth login
                                                    password)))))))))
