;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/replay/squid
  (:use #:cl)
  (:import-from #:util/macros
                #:def-easy-macro)
  (:local-nicknames (#:a #:alexandria)))
(in-package :screenshotbot/replay/squid)

(def-easy-macro with-squid (&binding host &fn fn)
  (let ((random-port (util/random-port:random-port)))
   (tmpdir:with-tmpdir (cache-dir)
     (tmpdir:with-tmpdir (log-dir)
      (uiop:with-temporary-file (:pathname squid.conf :type "conf" :prefix "squid"
                                 :stream squid.conf-stream)
        (uiop:with-temporary-file (:pathname allowed-sites :prefix "allowed_sites"
                                   :stream allowed-sites-stream)
          (write-squid.conf squid.conf-stream
                            :random-port random-port
                            :cache-dir cache-dir
                            :log-dir log-dir
                            :allowed-sites allowed-sites)

          (write-allowed-sites
           allowed-sites-stream)

          (with-open-file (squid-logs "/tmp/squid-logs"
                                      :if-exists :append
                                      :if-does-not-exist :create
                                      :direction :output)
            (let ((common-args (list
                                "-NYC"
                                "-d" "3"
                                "-n" "localsquid"
                                "-f" (namestring squid.conf))))
              (uiop:run-program (list* "/sbin/squid"
                                       "-z" common-args))
              (let ((process-info (uiop:launch-program
                                   (list* "/sbin/squid"
                                          common-args)
                                   :output :interactive
                                   :error-output :interactive)))
                (wait-for-port random-port)
                (unwind-protect
                     (let ((squid (format nil "~a:~d"
                                          "localhost"
                                          random-port)))
                       (log:info "Squid launched on ~S" squid)
                       (funcall fn squid))
                  (uiop:terminate-process process-info)
                  (log:info "Waiting for squid process to terminate")
                  (uiop:wait-process process-info)
                  (log:info "Squid process terminated")))))))))))

(defun write-squid.conf (stream &key
                                  random-port
                                  log-dir
                                  cache-dir
                                  allowed-sites)
  (format stream
          "http_port ~a
cache allow all
cache_log ~a
pid_filename ~a
cache_effective_user proxy
memory_cache_shared off
cache_dir aufs ~a 50000 16 256
acl whitelist dstdomain \"~a\"
http_access deny !whitelist"
          random-port
          (path:catfile log-dir "cache.log")
          (path:catfile log-dir "pid-file")
          (namestring cache-dir)
          (namestring allowed-sites))
  (finish-output stream))

(defun write-allowed-sites (stream)
  ;; TODO: we can do better. Since we're generating this dynamically,
  ;; we know exactly which IP address we need access to. But it'll
  ;; have to passed in as an argument.
  (format stream "
staging
replay

# static IP for staging
172.29.1.15

# second static IP for staging. It's unclear which network is used to
# connect from staging to the squid proxy since both are two networks.
172.20.0.6


# static IP for replay
172.29.1.14

# production over OpenVPN
10.9.8.1

# debian-us-east-001
45.33.86.76")
  (finish-output stream))

(defun wait-for-port (port)
  (loop for i from 0 to 50
        do
           (handler-case
               (let ((socket (usocket:socket-connect "127.0.0.1" port)))
                 (log:info "Got socket connection")
                 (usocket:socket-close socket)
                 (return))
             (usocket:socket-error (e)
               (log:info "Socket not ready yet: ~a" e)
               (sleep 0.1)))
        finally (error "Squid did not launch")))

#+nil
(with-squid (host)
  (log:info "Inside!"))
