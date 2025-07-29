(defpackage :pixel-diff/main
  (:use #:cl))
(in-package :pixel-diff/main)

(defun %main (&rest args)
  "Main entry point for the pixel-diff application"
  (when (< (length args) 2)
    (format t "Usage: pixel-diff <image1> <image2>~%")
    (format t "  Compare two images and display the differences~%")
    (return-from %main 1))
  
  (let ((image1 (first args))
        (image2 (second args)))
    (unless (probe-file image1)
      (format t "Error: Image file not found: ~a~%" image1)
      (return-from %main 1))
    
    (unless (probe-file image2)
      (format t "Error: Image file not found: ~a~%" image2)
      (return-from %main 1))
    
    (format t "Comparing images:~%  Before: ~a~%  After:  ~a~%" image1 image2)
    
    (let ((interface (pixel-diff/differ:create-empty-interface
                      :image1 (namestring image1)
                      :image2 (namestring image2))))
      (capi:display interface)
      0)))

(defun main ()
  (%main (cdr sys:*line-arguments-list*)))


