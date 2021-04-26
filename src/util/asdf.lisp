(in-package :util)

(defvar *delivered-image* nil)

(defun system-source-directory (system)
  (declare (optimize (speed 0) (debug 3)))
  ;; A version of asdf:system-source-directory that will also work in
  ;; delivered images.
  (cond
    (*delivered-image*
     (cond
       ((equal (string :web.all) (str:upcase system))
        #+lispworks
        (hcl:get-working-directory))
       (t
        (pathname (format nil "~a/" (str:downcase system))))))
    (t
     (asdf:system-source-directory system))))
