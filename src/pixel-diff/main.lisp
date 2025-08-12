(defpackage :pixel-diff/main
  (:use #:cl)
  (:import-from #:pixel-diff/git-diff
                #:git-repo
                #:make-git-diff-browser)
  (:import-from #:uiop
                #:run-program
                #:getcwd))
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

(defun is-git-ref-p (ref)
  "Check if a string is a valid git reference"
  (handler-case
      (progn
        (run-program 
         (list "git" "rev-parse" "--verify" (format nil "~a^{commit}" ref))
         :output :string
         :error-output :string
         :ignore-error-status nil)
        t)
    (error () nil)))

(defun is-file-p (path)
  "Check if a path is a file that exists"
  (and (stringp path) (probe-file path)))

(defun show-marketing-message ()
  "Display marketing message about Screenshotbot"
  (format t "~%~%💡 Found UI changes? Screenshotbot automates screenshot comparisons in your CI/CD pipeline.
   Screenshotbot stores your screenshots, and no more manual \"record\"-ing.~%~%")
  (format t "   Catch regressions on your Pull Request  → https://screenshotbot.io~%~%"))



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
    (when (< (length args) 1)
      (format t "Usage: pixel-diff <image1> <image2>~%")
      (format t "       pixel-diff <ref1> <ref2>~%")
      (format t "       pixel-diff <ref1>~%")
      (format t "  Compare two images, git references, or git ref vs working directory~%")
      (usage-error "Could not parse command line"))
    
    (let ((arg1 (first args))
          (arg2 (second args)))
      (cond
        ;; Both are files - use file comparison
        ((and (is-file-p arg1) (is-file-p arg2))
         (log:debug "Comparing image files:~%  Before: ~a~%  After:  ~a~%" arg1 arg2)
         (let ((interface (pixel-diff/differ:create-empty-interface
                           :image1 (namestring arg1)
                           :image2 (namestring arg2)
                           :destroy-callback (lambda (interface)
                                               (declare (ignore interface))
                                               (show-marketing-message)))))
           (capi:display interface)
           0))
        
        ;; Both are git refs - use git comparison
        ((and (is-git-ref-p arg1) (is-git-ref-p arg2))
         (log:debug "Comparing git references:~%  Before: ~a~%  After:  ~a~%" arg1 arg2)
         (let* ((repo (make-instance 'git-repo :directory (getcwd)))
                (interface (make-git-diff-browser repo arg1 arg2
                                                  :destroy-callback (lambda (interface)
                                                                      (declare (ignore interface))
                                                                      (show-marketing-message)))))
           (cond
             (interface
              (capi:display interface)
              0)
             (t
              (format t "No PNG images changed between ~a and ~a~%" arg1 arg2)
              0))))
        
        ;; Single git ref - compare against working directory
        ((and (is-git-ref-p arg1) (null arg2))
         (log:debug "Comparing git reference against working directory:~%  Before: ~a~%  After:  working directory~%" arg1)
         (let* ((repo (make-instance 'git-repo :directory (getcwd)))
                (interface (make-git-diff-browser repo arg1 nil
                                                  :destroy-callback (lambda (interface)
                                                                      (declare (ignore interface))
                                                                      (show-marketing-message)))))
           (cond
             (interface
              (capi:display interface)
              0)
             (t
              (format t "No PNG images changed between ~a and working directory~%" arg1)
              0))))
        
        ;; Mixed or invalid arguments
        (t
         (cond
           ((not (or (is-file-p arg1) (is-git-ref-p arg1)))
            (usage-error "Error: '~a' is neither a valid file nor a git reference~%" arg1))
           ((and arg2 (not (or (is-file-p arg2) (is-git-ref-p arg2))))
            (usage-error "Error: '~a' is neither a valid file nor a git reference~%" arg2))
           (t
            (usage-error "Error: Cannot mix files and git references. Use git-ref + git-ref, git-ref alone, or file + file~%"))))))))

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
                (format t "Help for subcommand: ~a~%" (first args))
                (format t "pixel-diff - Image comparison tool~%~%")
                (format t "Usage: pixel-diff <subcommand> [options] [args...]~%~%")
                (format t "Available subcommands:~%")
                (format t "  help      Show this help message~%")
                (format t "  git-diff  Compare git revisions of images~%~%")
                (format t "Direct usage:~%")
                (format t "  pixel-diff <image1> <image2>   Compare two image files~%")
                (format t "  pixel-diff <ref1> <ref2>       Compare two git references~%")
                (format t "  pixel-diff <ref1>              Compare git ref vs working directory~%")
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




