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
(ql:quickload "clsql-helper" :silent t)
(ql:quickload "colorize" :silent t)
(ql:quickload "tmpdir" :silent t)

#+screenshotbot-oss
(ql:quickload "bknr.datastore")

#+screenshotbot-oss
(ql:quickload "fiveam")


#+lispworks
(ql:quickload "slynk")

#+lispworks
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
  (loop for system in systems
        do
        (let ((res (ql:quickload system)))
          #+nil(format t "Got quickload result: ~S" res))))

;;(ql:quickload "auth")
;;(asdf:load-system "auth")

#-screenshotbot-oss
(ql:quickload "markup.test")

(eval `(setf ,(find-symbol "*IN-TEST-P*" "UTIL")
             t))

(defun main ()
  (tmpdir:with-tmpdir (tmpdir)
    (make-instance #-screenshotbot-oss
                   'util:safe-mp-store
                   #+screenshotbot-oss
                   'bknr.datastore:mp-store
                   :directory tmpdir
                   :subsystems (list (make-instance
                                      'bknr.datastore:store-object-subsystem)
                                     (make-instance
                                      'bknr.datastore:blob-subsystem)))
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
