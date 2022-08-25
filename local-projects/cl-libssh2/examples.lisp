(libssh2:with-ssh-connection sshc
    ("192.168.30.193"
     (libssh2:make-password-auth "root" "12345677")
     :hosts-db (namestring
                (merge-pathnames
                 (make-pathname :directory '(:relative ".ssh")
                                :name "libssh2-known_hosts")
                 (user-homedir-pathname))))
  (let ((local/remote "/tmp/image")
        (remote/local "/os-devel/BUILDER/builds/np1U/root.img")
        (stream/type  '(unsigned-byte 8)))
    (print "---1---")
    (libssh2:with-scp-input (in sshc remote/local stat)
      (with-open-file (out local/remote
                           :direction :output
                           :if-exists :supersede
                           :if-does-not-exist :create
                           :element-type stream/type)
        (cl-fad:copy-stream in out)))
    (print "---2--")
    (with-open-file (in local/remote
                        :direction :input
                        :element-type stream/type)
      (libssh2:with-scp-output (out sshc local/remote
                                    (file-length in))
        (cl-fad:copy-stream in out)))
    (print "---3---")
    (libssh2:with-execute* (in sshc (format nil "md5sum ~a ~a"
                                            remote/local
                                            local/remote))
      (loop for line = (read-line in nil)
         while line do (print line)))))
