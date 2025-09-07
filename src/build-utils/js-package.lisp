;; Copyright 2018-Present Modern Interpreters Inc.
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :build-utils/js-package
  (:use #:cl
        #:asdf
        #:alexandria)
  (:import-from :build-utils/common
                #:find-transitive-system-deps)
  (:export #:web-asset
           #:js-system
           #:js-library
           #:js-file
           #:js-file
           #:ps-file))
(in-package :build-utils/js-package)

(defclass web-asset ()
  ())

(defclass js-library (asdf:system)
  ())

(defclass js-system (js-library web-asset)
  ())

(defclass js-file (asdf:static-file)
  ()
  (:default-initargs :type "js"))

(defclass ps-file (asdf:static-file)
  ()
  (:default-initargs :type "lisp"))

(defmethod js-lib-input-files ((o compile-op) (m module))
  (loop for x in (component-children m) appending
        (js-lib-input-files o x)))

(defmethod js-lib-input-files ((o compile-op) (j js-file))
  (asdf:input-files o j))

(defmethod js-lib-input-files ((o compile-op) (p ps-file))
  (asdf:output-files o p))

(defmethod js-lib-input-files ((o compile-op) (j js-library))
  (call-next-method))

(defmethod js-input-files ((o compile-op) (j js-system))
  (let ((libraries (find-transitive-system-deps j 'js-library)))
    (loop for lib in libraries
          appending (js-lib-input-files o lib))))

(defmethod asdf:output-files ((o compile-op) (j js-system))
  (list
   ;; okay, where does this go?
   (format nil "~a.js" (string-downcase (component-name j)))
   (format nil "~a.js.map" (string-downcase (component-name j)))))

(defmethod asdf:output-files ((o compile-op) (p ps-file))
  (list
   (format nil "~a.js" (string-downcase (component-name p)))))

(defmethod asdf:perform ((o compile-op) (p ps-file))
  (uiop:with-staging-pathname (output-file (asdf:output-file o p))
   (with-open-file (out output-file :direction :output
                                    :if-exists :supersede)
     (write-string
      (ps:ps-compile-file (car (asdf:input-files o p)))
      out))))

(defun guess-java ()
  (loop for path in (list
                     "/opt/homebrew/opt/openjdk/bin/java"
                     "/usr/bin/java")
        if (probe-file path)
          return path))

(defmethod asdf:perform ((o compile-op) (j js-system))
  (restart-case
      (let ((input-files (remove-duplicates (js-input-files o j) :test 'equal :from-end t)))
        ;;(format *error-output* "Compling: ~S" input-files)
        (loop for x in input-files
              do
                 (unless (probe-file x)
                   (error "The file ~A does not exist" x)))
        (multiple-value-bind  (out err ret)
            (uiop:run-program
             (append
              (list
               #-darwin "java"
               #+darwin (guess-java)
               "-jar"
               (namestring
                (asdf:system-relative-pathname
                 :build-utils "closure-compiler/closure-compiler-v20240317.jar"))
               "--js_output_file"
               (namestring (car (output-files o j)))
               "--create_source_map"
               (namestring (cadr (output-files o j))))
              (loop for filename in input-files
                    appending (list "--js" (namestring filename))))
             :output 'string
             :error-output 'string
             :ignore-error-status t)
          (unless (= ret 0)
            (error "Could not compile js assets: ~%~%stdout:~a~%~%stderr:~%~a~%" out err)))
        (with-open-file (s (car (output-files o j)))
         (format t "File size is: ~A" (file-length s))))
    (retry-perform ()
      (perform o j))))


;; (get-js-component "/home/arnold/builds/web/src/common/bootstrap-5.0.0-beta2/js/src/")
;;
;; At time of writing, we haven't used this to actually generate any
;; asd files, so it might be buggy.
(defun get-js-component (dir)
  (cond
    ((path:-d dir)
     (let ((name (car (last (pathname-directory dir)))))
       `(:module ,name :components (,@ (remove-if 'null
                                                   (loop for x in (fad:list-directory dir)
                                                         collect (get-js-component x)))))))
    ((equal "js" (pathname-type dir))
     `(,(let ((*package* :cl-user)) (format nil "~s" 'js-file)) ,(pathname-name dir)))))
