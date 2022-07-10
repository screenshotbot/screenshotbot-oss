(defpackage :test-runner/test-runner
  (:nicknames :test-runner)
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:init
   #:main))
(in-package :test-runner/test-runner)

(defun init-jvm-for-ccl ()
  (progn
  (funcall (find-symbol "JVM-INIT" "JVM"))
  (setf *debugger-hook* nil)))

(defun hide-outputs ()
  (setf fiveam:*test-dribble* *standard-output*)
  (let ((null-file (open (if (uiop:os-windows-p) "nul" "/dev/null") :direction :output
                                                                    :if-exists :supersede)))
    (setf *standard-output* null-file)
    (setf *error-output* null-file)
    (setf *trace-output* null-file)
    (log:config :fatal)))

(defun clean-up-screenshotbot-screenshots ()
  (let ((dir (asdf:system-relative-pathname :screenshotbot "static-web-output/")))
    (when (path:-d dir)
      (uiop:delete-directory-tree dir
                                  :validate (lambda (x)
                                              (declare (ignore x))
                                              t))
      (asdf:compile-system :screenshotbot.css-assets)
      (let ((default-css (path:catfile dir "assets/css/default.css")))
        (ensure-directories-exist default-css)
        (uiop:copy-file
         (car (asdf:output-files 'asdf:compile-op :screenshotbot.css-assets))
         default-css)))))

(defun init ()
  #+ccl
  (init-jvm-for-ccl)
  (load-systems)
  (unless (equal "1" (uiop:getenv "TDRHQ_TEST_DEBUG"))
    (hide-outputs))
  (clean-up-screenshotbot-screenshots))

(defun find-tests ()
  (let* ((pathname "src/")
         (index (ql::ensure-system-index pathname)))
    (when index
      (with-open-file (stream index)
        (remove-if 'null
                    (loop for line = (read-line stream nil)
                          while line
                          collect
                          (let ((system (pathname-name (pathname line))))
                            (cond
                              ((member "build" (pathname-directory line)
                                       :test 'equal)
                               ;; bad test, discard
                               (values))
                              ((str:ends-with-p ".tests" system)
                               system)
                              ((str:ends-with-p ".test" system)
                               system)
                              (t
                               (let ((x (asdf:find-system (format nil "~a/tests" system) nil)))
                                 (when x
                                   (asdf:component-name x))))))))))))

(defun load-systems ()
  (let ((systems (or
                  #+lispworks
                  (let ((pos (position "-system" system:*line-arguments-list* :test 'equal)))
                    (when pos
                      (str:split "," (elt system:*line-arguments-list* (1+ pos)))))
                  (find-tests))))
    #-screenshotbot-oss
    (progn
      (push "markup.test" systems)
      #-ccl
      (when (uiop:getenv "JENKINS_URL")
        (let ((files (uiop:read-file-lines "build/affected-files.txt")))
          (log:info "Got affected files ~S" files)
          (setf systems (testing/affected-systems:filter-affected-systems
                         systems
                         files)))))
    (log:info "Running the following tests: ~S" systems)

    (dolist (system systems)
      (log:info "Loading: ~s" system)
     (ql:quickload system))))

(defun main ()
  (tmpdir:with-tmpdir (tmpdir)
    ;; on CCL, the JVM is already loaded before the main systems
    #+lispworks
    (unless (position "-no-jvm" system:*line-arguments-list* :test #'equal)
      (jvm:jvm-init))
    (fiveam:test foo-bar
      (fiveam:is-true (equal "foo" "foo")))
    (if (not (fiveam:run-all-tests))
        (uiop:quit 1)))
  (uiop:quit 0))
