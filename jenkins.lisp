;; Copyright 2018-Present Modern Interpreters Inc.
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

;; these quickloads are required because we need to load them before
;; we set dspec:*redefinition-action* to :error

(ql:quickload "babel" :silent t)

#-screenshotbot-oss
(ql:quickload "clsql" :silent t)

#-screenshotbot-oss
(ql:quickload "testing")

(ql:quickload "fiveam")

(ql:quickload "colorize" :silent t)
(ql:quickload "tmpdir" :silent t)

#+screenshotbot-oss
(ql:quickload "bknr.datastore")

#+screenshotbot-oss
(ql:quickload "fiveam")


(ql:quickload :cl-markdown)

#+lispworks
(ql:quickload "slynk")

#+(and :lispworks (not :mswindows))
(ql:quickload :osicat :silent t)

;; #+lispworks
;; (setf dspec:*redefinition-action* :error)

(ql:quickload :jvm)

#+ccl
(progn
  (funcall (find-symbol "JVM-INIT" "JVM"))
  (setf *debugger-hook* nil))

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
    (ql:quickload
     system)))

;;(ql:quickload "auth")
;;(asdf:load-system "auth")

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
      default-css))))


(eval `(setf ,(find-symbol "*IN-TEST-P*" "UTIL")
             t))


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

#+lispworks
(mp:initialize-multiprocessing :main nil #'main)

#-lispworks
(main)
