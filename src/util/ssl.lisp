(defpackage :util/ssl
  (:use #:cl)
  (:export
   #:*intern-ssl-context*))
(in-package :util/ssl)

(defvar *certificate*
  "-----BEGIN CERTIFICATE-----
MIIDWzCCAkOgAwIBAgIUWzoGdurqWqw5e0eQh43pJkOBgAYwDQYJKoZIhvcNAQEL
BQAwPTELMAkGA1UEBhMCVVMxCzAJBgNVBAgMAk5KMSEwHwYDVQQKDBhJbnRlcm5l
dCBXaWRnaXRzIFB0eSBMdGQwHhcNMjMxMTIwMTg1NTU1WhcNMjQxMTE5MTg1NTU1
WjA9MQswCQYDVQQGEwJVUzELMAkGA1UECAwCTkoxITAfBgNVBAoMGEludGVybmV0
IFdpZGdpdHMgUHR5IEx0ZDCCASIwDQYJKoZIhvcNAQEBBQADggEPADCCAQoCggEB
ALBpkNcm73b6db+kiZJL/GqHvKo+QK2ZuvunyoMtsCdQ2DXYc4Xr1+FySo9rMuQE
tbCrhn8YAffalAWWXU664XaIOwRATEQPc5yvfW7WYcmsSK3tE6+Aq99mKqpT0X7A
pCQxr2b/OjPdv3d5iE9+nPKatsPwMvk+A32eKzxDRSpxTPUp4aSqvAz6xGw955Oe
b7uj/Mwy+8+nYQzAkuLb95p0dbRaQrrY0gVXA7G5rdOMY32ivixhtKVAz6iVbWsf
gb7zJbU/pQ/ajhgS9fmZ5VPqcobRP2ib1B3l7Yybh1vBJeV+XCZtY+R7VtwM3tmP
rQyNSpxftnSmCD1GttE4JUkCAwEAAaNTMFEwHQYDVR0OBBYEFNk1fEuOj2faM73c
HmUf9N0iDWHrMB8GA1UdIwQYMBaAFNk1fEuOj2faM73cHmUf9N0iDWHrMA8GA1Ud
EwEB/wQFMAMBAf8wDQYJKoZIhvcNAQELBQADggEBAFAq9Ck3pYiAJ/fxlOMmjuBN
enh6KnkwGy5f5PYgQw8l/LpCv9iHDBzJjc4mFULXp+H+wdK8ohQDwdd7brpIBdTi
x9almhC7lVucjs7SaNUDg8h0O+1B/iUDst8X1ixOa4osGZRf2g9/c3BrBknUvuQ8
VMPXbGznmwXZS969g3zEmhMfRxl/4O5G5Hd2n9uYj0nWD5N9L3B8cuJd6etI6ZZC
1P0L+QIlEW9Y8DuaKlDC/VjxqZmmAKmzAISEDK5VZ/kV/RhpD8OavWfIQS8vZhTM
a1yOubCHt09QsDkyvBTWP3VX541g1voHDCcYMOyzrdv/KQHfwl/ojnEAg1t9HzA=
-----END CERTIFICATE-----")

(defvar *local-cert*)

(defun read-x509-from-file (file)
  (comm:pem-read "X509" file))

(defun read-x509-from-string (str)
  (uiop:with-temporary-file (:pathname p :stream s)
    (write-string str s)
    (finish-output s)
    (comm:pem-read "X509" p)))

(defun local-cert ()
  (if (boundp '*local-cert*)
      *local-cert*
      (setf
       *local-cert*
       (read-x509-from-file "~/.tunnel/certificate.pem"))))

(defvar *cert-x509* nil)

(defun cert-x509 ()
  (util/misc:or-setf
   *cert-x509*
   (read-x509-from-string *certificate*)))

(fli:define-foreign-function (x509-cmp "X509_cmp")
    ((a (:pointer :void) #| just convenience, it's actually an
        x509-pointer, but pem-read doesn't reutn
        that |#)
     (b comm:x509-pointer))
  :result-type :int)

(defun allowed-cert-p (cert &key allowed-certs )
  (let ((allowed-certs (or allowed-certs
                           (list
                            (cert-x509)
                            (local-cert)))))
    (some
    (lambda (allowed)
      (when allowed
        (= 0
           (x509-cmp allowed
                     cert))))
    (remove-if #'null
               allowed-certs))))

(defun verify-callback (socket-stream &key allowed-certs)
  (let ((certs (comm:ssl-connection-copy-peer-certificates socket-stream)))
    (unwind-protect
        (when certs
          (loop for cert across certs
               if (not (allowed-cert-p cert :allowed-certs allowed-certs))
                 return nil
               finally
                  (return t)))
      (comm:release-certificates-vector certs))))

(defvar *intern-ssl-context*
  (comm:create-ssl-client-context :verify-callback #'verify-callback
                                  :implementation :openssl
                                  :openssl-trusted-file :default
                                  :openssl-trusted-directory :default))

(defvar *context-cache* (make-hash-table :test #'equal)
  "For a given SSL certificate, cache of the corresponding context")

(defun make-ssl-context (&key certificate)
  (util/misc:or-setf
   (gethash certificate *context-cache*)
   (let ((x509 (read-x509-from-string certificate)))
     (comm:create-ssl-client-context
      :implementation :openssl
      :verify-callback (lambda (socket-stream)
                         (verify-callback socket-stream
                                          :allowed-certs
                                          (list x509)))
      :openssl-trusted-file :default
      :openssl-trusted-directory :default))))
