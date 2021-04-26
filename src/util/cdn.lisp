(defpackage #:util.cdn
  (:use #:cl )
  (:export
   :make-cdn
   :*cdn-domain*
   :update-key
   :*cdn-cache-key*))
(in-package #:util.cdn)

;; to invalidate the cache reload

(progn
  (defvar *cdn-cache-key* nil)
  (defun update-key ()
    (setf *cdn-cache-key* (secure-random:number 1000000000)))
  (unless *cdn-cache-key*
   (update-key)))

(defvar *cdn-domain* nil)

(defun make-cdn (href)
  (cond
    ((and *cdn-domain*
          (not util:*delivered-image*)
          (str:starts-with? "/" href)
          (not (str:starts-with? "//" href)))
     (util:add-get-param-to-url
      (format nil "~a~a" *cdn-domain* href)
      "cache-key"
      (format nil "~a" *cdn-cache-key*)))
    (t
     (util:add-get-param-to-url
      href
      "cache-key"
      (format nil "~a" *cdn-cache-key*)))))
