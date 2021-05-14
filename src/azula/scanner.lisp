(defpackage :azula/scanner
  (:use :cl
   :alexandria)
  (:import-from :azula/main
   :azula-root
                :load-build-file
                :*targets*))
(in-package :azula/scanner)

(defvar *current-build-file*)

(defvar *azula* "AZULA")

(defun join-azula-dirs (dir1 name)
  (cond
    ((equal dir1 "//")
     (format nil "~a~a" dir1 name))
    (t
     (format nil "~a/~a" dir1 name))))

(defun scan ()
  (let (new-targets)
   (let ((*targets* nil))
     (labels ((%scan (directory prefix)
                (log:debug "Scanning: ~S ~S" directory prefix)
                (let ((build-file (path:catfile directory *azula*)))
                  (when (path:-e build-file)
                    (load-build-file build-file)))
                (unless (equal prefix "//build")
                  (dolist (dir (fad:list-directory directory))
                    (when (path:-d dir)
                      (%scan dir (join-azula-dirs prefix (car (last (pathname-directory dir))))))))))
       (%scan (azula-root) "//")
       (setf new-targets *targets*)))
    (setf *targets* new-targets)))

;; (scan)
