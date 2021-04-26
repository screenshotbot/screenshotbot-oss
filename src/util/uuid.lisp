(in-package :util)

(defun safe-uuid ()
  (uuid:print-bytes nil (uuid:make-v4-uuid)))

(defun make-secret-code ()
  (base32:bytes-to-base32 (secure-random:bytes 32
                                               secure-random:*generator*)))
