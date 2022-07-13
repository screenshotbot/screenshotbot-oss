(defpackage :screenshotbot/hub/server
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:hub))
(in-package :screenshotbot/hub/server)

(defclass local-hub ()
  ())

(defparameter +json-content-type+
  "application/json; charset=UTF-8")

(defvar *hub* (make-instance 'local-hub))

(defun hub ()
  *hub*)

(auto-restart:with-auto-restart ()
 (defmethod request-session-and-respond ((hub local-hub)
                                         (arguments string))
   (multiple-value-bind (data ret)
       (util/request:http-request
        (format nil "http://localhost:4444/wd/hub/session")
        :method :post
        :want-string t
        :content-type +json-content-type+
        :content arguments
        :external-format-out :utf-8)
     (assert (not (eql ret 500)))
     (setf (hunchentoot:return-code*) ret)
     (setf (hunchentoot:content-type*) +json-content-type+)
     data)))
