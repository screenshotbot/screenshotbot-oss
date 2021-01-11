(defpackage #:pem/parser
  (:use #:cl)
  (:import-from #:cl-ppcre
                #:create-scanner
                #:register-groups-bind)
  (:export #:parse
           #:parse-file))
(in-package #:pem/parser)

(defun parse (pem)
  (assert (eq (stream-element-type pem) #+lispworks 'base-char
                                        #-lispworks 'character))
  (loop for line = (read-line pem nil nil)
        while line
        collect (ppcre:register-groups-bind (label)
                    ("^-----BEGIN (.+)-----" line)
                  (let ((end (ppcre:create-scanner (format nil "-----END ~A-----" label))))
                    (loop for line = (read-line pem)
                          if (ppcre:scan end line)
                            do (return (cons label
                                             (format nil "~{~A~^~%~}" data)))
                          else
                            collect line into data)))))

(defun parse-file (pem)
  (check-type pem pathname)
  (with-open-file (in pem)
    (parse in)))
