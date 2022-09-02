(defpackage :hunchentoot-extensions/postdata
  (:use #:cl
        #:hex)
  (:local-nicknames (#:a #:alexandria)))
(in-package :hunchentoot-extensions/postdata)

(defun write-postdata-to-file (file-name)
  (uiop:with-staging-pathname (file-name)
    (with-open-file (output file-name :direction :output
                                      :element-type '(unsigned-byte 8)
                                      :if-exists :supersede)
      (let ((content-length (parse-integer (hunchentoot:header-in* :content-length))))
        (let ((buf (make-array 4096 :element-type '(unsigned-byte 8))))
          (let ((input (hunchentoot:raw-post-data :force-binary t
                                                  :want-stream t)))
            (loop while (> content-length 0) do
              (let ((bytes (read-sequence buf input)))
                (write-sequence buf output :end bytes)
                (decf content-length bytes)))))))))
