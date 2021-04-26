(in-package :util)

(defun download-file (url pathname)
  (multiple-value-bind (input resp args) (drakma:http-request url
                                                              :force-binary t
                                                              :want-stream t)
    (unless (eql 200 resp)
      (error "bad code"))
    (with-open-file (output pathname :direction :output :if-exists :supersede :if-does-not-exist :create
                            :element-type '(unsigned-byte 8))
      (fad:copy-stream  input output))))
