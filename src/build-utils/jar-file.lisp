;; Copyright 2018-Present Modern Interpreters Inc.
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :build-utils/jar-file
  (:use :cl
   :alexandria
   :asdf)
  (:export :java-library
   :jar-file
   :java-class-path
   :java-file))
(in-package :build-utils/jar-file)

(defclass java-library (asdf:system)
  ())

(defclass jar-file (asdf:static-file)
  ((asdf::type :initform "jar")))

(defclass java-file (asdf:source-file)
  ((asdf::type :initform "java")))

(defun java-class-path (system)
  (append
   (typecase system
     (java-library
      (cons
       (car (output-files 'compile-op system))
       (loop for x in (component-children system)
             if (typep x 'jar-file)
               collect (component-pathname x)))))
   (compile-class-path system)))

(defun compile-class-path (system)
  (apply 'append
   (mapcar 'java-class-path
            (mapcar (lambda (x) (find-system x))
                    (system-depends-on system)))))

(defmethod output-files ((o compile-op) (s java-library))
  (let ((name (string-downcase (component-name s))))
    (mapcar 'pathname
     (list (format nil "~a.jar" name)
           (format nil "~a-classes/" name)))))

(defun all-java-files (component)
  (loop for x in (component-children component)
        if (typep x 'parent-component)
          append (all-java-files x)
        if (typep x 'java-file)
          collect (component-pathname x)))

(defun join (sep list)
  (let ((firstp t))
   (let ((output (make-string-output-stream)))
     (dolist (x list)
       (cond
         (firstp
          (format output "~a" x)
          (setf firstp nil))
         (t
          (format output "~a~a" sep x))))
     (get-output-stream-string output))))

(defmethod asdf:perform ((o compile-op) (s java-library))
  (destructuring-bind (jar dir) (output-files o s)
    (when (uiop:directory-exists-p dir)
      (uiop:delete-directory-tree dir :validate (lambda (x)
                                                  (or
                                                   (equal nil (pathname-name x))
                                                   (equal "class" (pathname-type x))))))
    (ensure-directories-exist dir)
    (cond
      ((all-java-files s)
       (multiple-value-bind (output err ret)
           (uiop:run-program `("javac" "-d" ,(namestring dir)
                                       "-cp" ,(join ":" (mapcar 'namestring (compile-class-path s)))
                                       ,@(mapcar 'namestring (all-java-files s)))
                             :ignore-error-status t
                             :output 'string
                             :error-output 'string)
         (unless (eql ret 0)
           (error "Failed to compile java code: ~%stderr: ~%~a~%stdout: ~%~a" err output)))))
    (when (path:-e jar)
     (delete-file jar))
    (uiop:run-program (list "jar"
                            "cf" (namestring jar)
                            "-C" (namestring dir)
                            "."))))

(defmethod asdf:perform ((o compile-op) (s java-file))
  ;; do nothing! All compilation is on the system level
  nil)

(defmethod asdf:perform ((o load-op) (s java-file))
  nil)

;; (all-java-files (find-system :java.main))
;; (compile-system :java.main)
;; (compile-class-path (find-system :java.main))
