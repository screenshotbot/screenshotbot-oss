;;; -*- mode: lisp; syntax: common-lisp; indent-tabs-mode: nil -*-

(in-package #:libssh2.test)

(in-suite integration)

(deftest scp-copy-back-and-forth ()
  (with-ssh-connection sshc
     (*test-host*
     (libssh2:make-password-auth *user1* *password1*)
     :hosts-db *known-hosts-path*)
    (let ((test-file (asdf:system-relative-pathname (asdf:find-system :libssh2) "test/data/testfile.tgz"))
          (remote-name "/tmp/copied-to-remote.tgz")
          (final "/tmp/copied-back-from-remote.tgz")
          (md5 "3fee5a92e7d3a2c716e922434911aa7c"))
      ;; copy file to ssh host
      (scp-put test-file remote-name)
      ;; copy back from ssh host
      (scp-get remote-name final)
      ;; calculate remote md5
      (libssh2:with-execute* (in sshc (format nil "md5sum ~a" test-file))
        (let ((sums (loop for line = (read-line in nil)
                          while line
                          do (format t "~%<~A>~%" (split-sequence:split-sequence #\Space line))
                          collect (car (split-sequence:split-sequence #\Space line)))))
          (is (every (lambda (s) (equal s md5)) sums) "MD5 sums of local and remote files differ"))))))
