(in-package :bknr.utils)

(defvar *capability-count* 0)

(defun make-capability-string ()
  "return new unique capabilty as a string"
  (md5-string (format nil "~a.~a" (get-universal-time)
		      (incf *capability-count*))))
