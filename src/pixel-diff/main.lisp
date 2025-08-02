(defpackage :pixel-diff/main
  (:use #:cl))
(in-package :pixel-diff/main)

(define-condition usage-error (error)
  ((format-string :initarg :format-string :reader usage-error-format-string)
   (arguments :initarg :arguments :reader usage-error-arguments))
  (:report (lambda (condition stream)
             (apply #'format stream 
                    (usage-error-format-string condition)
                    (usage-error-arguments condition)))))

(defun usage-error (format-string &rest arguments)
  "Signal a usage-error condition with formatted message"
  (error 'usage-error 
         :format-string format-string 
         :arguments arguments))



(defun git-diff-command ()
  (clingon:make-command
   :name "git-diff"
   :description "Compare git revisions of images"
   :usage "git-diff <image-path> [revision1] [revision2]"
   :options (list)
   :handler (lambda (cmd)
              (declare (ignore cmd))
              (error "unimplemented"))))

(defun pixel-diff-command/handler (cmd)
  (let ((args (clingon:command-arguments cmd)))
    (when (< (length args) 2)
      (format t "Usage: pixel-diff <image1> <image2>~%")
      (format t "  Compare two images and display the differences~%")
      (usage-error "Could not parse command line"))
    
    (let ((image1 (first args))
          (image2 (second args)))
      (unless (probe-file image1)
        (usage-error "Error: Image file not found: ~a~%" image1))
      
      (unless (probe-file image2)
        (usage-error "Error: Image file not found: ~a~%" image2))
      
      (format t "Comparing images:~%  Before: ~a~%  After:  ~a~%" image1 image2)
      
      (let ((interface (pixel-diff/differ:create-empty-interface
                        :image1 (namestring image1)
                        :image2 (namestring image2))))
        (capi:display interface)
        0))))

(defun pixel-diff-command ()
  (clingon:make-command
   :name "pixel-diff"
   :description "Compare two images and display the differences"
   :usage "pixel-diff <subcommand> [options] [args...]"
   :options (list)
   :sub-commands (list (git-diff-command)
                       (help-command))
   :handler #'pixel-diff-command/handler))

(defun help-command ()
  (clingon:make-command
   :name "help"
   :description "Show help information"
   :usage "help [subcommand]"
   :options (list)
   :handler (lambda (cmd)
              (let ((args (clingon:command-arguments cmd)))
                (if args
                    (format t "Help for subcommand: ~a~%" (first args))
                    (progn
                      (format t "pixel-diff - Image comparison tool~%~%")
                      (format t "Usage: pixel-diff <subcommand> [options] [args...]~%~%")
                      (format t "Available subcommands:~%")
                      (format t "  help      Show this help message~%")
                      (format t "  git-diff  Compare git revisions of images~%~%")
                      (format t "Direct usage: pixel-diff <image1> <image2>~%")
                      (format t "  Compare two images directly~%")))
                0))))


(defun %main (args)
  "Main entry point for the pixel-diff application"
  (let ((app (pixel-diff-command)))
    (clingon:run app args)))

(defun main ()
  (%main (cdr sys:*line-arguments-list*)))

(defun test-%main ()
  "Test function for %main using example images"
  (let ((image1 (namestring (asdf:system-relative-pathname :pixel-diff "examples/image1.png")))
        (image2 (namestring (asdf:system-relative-pathname :pixel-diff "examples/image2.png"))))
    (unless (probe-file image1)
      (format t "Error: Test image not found: ~a~%" image1)
      (return-from test-%main 1))
    
    (unless (probe-file image2)
      (format t "Error: Test image not found: ~a~%" image2)
      (return-from test-%main 1))
    
    (format t "Running test with example images...~%")
    (%main (list image1 image2))))

;; (test-%main)




