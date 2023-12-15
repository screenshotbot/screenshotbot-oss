(defpackage :test-runner/test-runner
  (:nicknames :test-runner)
  (:use #:cl)
  (:import-from #:util/misc
                #:with-global-binding)
  (:import-from #:util/threading
                #:*log-sentry-p*)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:init
   #:main
   #:image-main))
(in-package :test-runner/test-runner)

(defvar *test-trace-io* nil)
(defun init-jvm-for-ccl ()
  (progn
  (funcall (find-symbol "JVM-INIT" "JVM"))
  (setf *debugger-hook* nil)))

#-(or jipr screenshotbot-oss eaase-oss)
(setf sentry:*disabledp* t)

(defun hide-outputs ()
  (setf fiveam:*test-dribble* *standard-output*)
  (setf *test-trace-io* *trace-output*)
  #-ccl ;; for some reason breaks some tests on ccl
  (let ((null-file
          (open "build/test-logs"
                :direction :output
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
                                              t)))))

(defun init ()
  #+ccl
  (init-jvm-for-ccl)
  (load-systems)
  (clean-up-screenshotbot-screenshots))

(defun find-tests ()
  (append
   (%find-tests "src/")
   (%find-tests "third-party/bknr.datastore/")))

(defun %find-tests (pathname)
  (when (path:-d pathname)
   (let* ((index (ql::ensure-system-index pathname)))
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
                                   (asdf:component-name x)))))))))))))


(defun screenshot-tests ()
  "Tests that generate screenshots should always be run"
  (list
   #- (or eaase-oss)
   "screenshotbot/tests"
   #- (or screenshotbot-oss eaase-oss)
   "screenshotbot.pro/tests"))

(defun load-systems ()
  (let ((systems (or
                  (let ((pos (position "-system" (uiop:raw-command-line-arguments) :test 'equal)))
                    (when pos
                      (str:split "," (elt (uiop:raw-command-line-arguments) (1+ pos)))))
                  (union
                   (find-tests)
                   (screenshot-tests)
                   :test #'string-equal)))
        (test-redefinitions-p (not (str:emptyp (uiop:getenv "TDRHQ_TEST_REDEF")))))
    #- (or screenshotbot-oss eaase-oss)
    (progn
      #-ccl
      (when (uiop:getenv "JENKINS_URL")
        (let ((files (uiop:read-file-lines "build/affected-files.txt")))
          (log:info "Got affected files ~S" files)
          (setf systems (test-runner/affected-systems:filter-affected-systems
                         systems
                         files)))))
    (log:info "Running the following tests: ~S" systems)

    #+lispworks
    (when test-redefinitions-p
      (ql:quickload :hu.dwim.def+hu.dwim.common)
      (ql:quickload :hu.dwim.def/namespace)
      (ql:quickload :slynk/fancy-inspector)
      (ql:quickload :slynk/indentation)
      ;; We might not need this in the future
      (ql:quickload :adw-charting-vecto)
      (ql:quickload :osicat)
      (ql:quickload :colorize)
      (ql:quickload :clsql)
      (ql:quickload :zpb-exif)

      (setf dspec:*redefinition-action* :error))
    (dolist (system systems)
      (log:info "Loading: ~s" system)
      (ql:quickload system))
    #+lispworks
    (setf dspec:*redefinition-action* :warn)))

(defun maybe-hide-outputs ()
  (unless (equal "1" (uiop:getenv "TDRHQ_TEST_DEBUG"))
    (hide-outputs)))

#+lispworks
(defun debugger-hook (condition old-hook)
  "On LW <= 8.0.1 there's a bug that causes LW to crash when a thread
fails."
  (format *test-trace-io* "A background thread crashed:~%")
  (dbg:output-backtrace :brief *test-trace-io*)
  (invoke-restart 'cl:abort))

(defun call-with-main-wrapper (fn)

  #+lispworks
  (setf *debugger-hook* 'debugger-hook)

  (tmpdir:with-tmpdir (tmpdir)
    ;; on CCL, the JVM is already loaded before the main systems
    #+(and lispworks (not jipr) (not eaase-oss))
    (unless (position "-no-jvm" system:*line-arguments-list* :test #'equal)
      (jvm:jvm-init))
    (funcall fn))
  (uiop:quit 0))

(defun maybe-profile (fn)
  #-lispworks
  (funcall fn)
  #+lispworks
  (if (position "-profile" system:*line-arguments-list*)
      (hcl:profile
       (funcall fn))
      (funcall fn)))

(defun safely-run-all-tests ()
  (fiveam:test foo-bar
    (fiveam:is-true (equal "foo" "foo")))
  (if (not (maybe-profile
            (lambda ()
             (fiveam:run-all-tests))))
      (uiop:quit 1)))

(defun main ()
  (with-global-binding ((*log-sentry-p* nil))
    (call-with-main-wrapper
     (lambda ()
       (maybe-hide-outputs)
       (safely-run-all-tests)))))

(defun fix-system-name (system)
  (let ((system (if (str:starts-with-p ":" system)
                    (str:upcase (str:substring 1 nil system))
                    system)))
    system))

(defun guess-fiveam-suite (system)
  (let ((system (str:upcase system)))
    (let ((suite-name
            (str:replace-all "." "/"
             (if (str:ends-with-p "/TESTS" system)
                 (str:replace-all "/TESTS" "" system)
                 system))))
      (let ((guessed (intern suite-name "KEYWORD")))
        (format t "Guessed suite name as: ~a" guessed)
        guessed))))

(defun image-main ()
  "The main function when called the test-runner is saved to build/t"
  (call-with-main-wrapper
   (lambda ()
     #+lispworks
     (let ((systems (loop for name in (cdr system:*line-arguments-list*)
                          if (not (eql #\- (elt name 0)))
                            collect name)))
       (format t "Running tests: ~S~%" systems)
       (dolist (system systems)
         (ql:quickload (fix-system-name system)))
       (format t "Loaded tests~%" systems)
       (maybe-hide-outputs)
       (loop for system in systems
             collect (fiveam:run (guess-fiveam-suite system))
             into all-results
             finally
                (unless (fiveam:explain! (alexandria:flatten all-results))
                  (uiop:quit 1)))))))
