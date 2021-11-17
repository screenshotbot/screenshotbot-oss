(defpackage :util/emacs
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:list-packages-for-symbol))
(in-package :util/emacs)

(defun list-packages-for-symbol (name)
  (mapcar
   #'package-name
     (loop for package in (list-all-packages)
           if (a:when-let (sym (find-symbol (string-upcase name)
                                            package))
                (eql package (symbol-package sym)))
             collect package)))
