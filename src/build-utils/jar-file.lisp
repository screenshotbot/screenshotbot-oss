;; Copyright 2018-Present Modern Interpreters Inc.
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :build-utils/jar-file
  (:use #:cl
        #:alexandria
        #:build-utils/common
        #:tmpdir
        #:asdf)
  (:import-from #:build-utils/remote-file
                #:remote-file)
  (:export #:java-library
           #:jar-file
           #:java-class-path
           #:java-file
           #:jar-resource
           #:remote-jar-file
           #:jar-bundle-op
           #:collect-runtime-jars
           #:safe-run-program))
(in-package :build-utils/jar-file)

(defclass provides-jar ()
  ()
  (:documentation "A component that can provide a jar file"))

(defclass java-library (asdf:system provides-jar)
  ((compile-depends-on :initarg :compile-depends-on
                       :reader compile-depends-on)))

(defmethod jar ((lib java-library))
  (car (output-files 'compile-op lib)))

(defclass jar-file (asdf:static-file provides-jar)
  ()
  (:default-initargs :type "jar"))

(defclass remote-jar-file (remote-file provides-jar)
  ()
  (:default-initargs :remote-file-type "jar"))

(defmethod jar ((s remote-jar-file))
  (car (output-files 'compile-op s)))

(defclass jar-resource (asdf:static-file)
  ()
  (:default-initargs :type nil))

(defclass java-file (asdf:source-file)
  ()
  (:default-initargs :type "java"))

(defmethod jar ((x jar-file))
  (component-pathname x))

(defun java-class-path (system)
  (append
   (typecase system
     (java-library
      (cons
       (car (output-files 'compile-op system))
       (loop for x in (component-children system)
             if (typep x 'provides-jar)
               collect (jar x))))
     (remote-jar-file
      (output-files 'compile-op system)))
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

(defun all-resource-files (component)
  (loop for x in (component-children component)
        appending
        (etypecase x
          (remote-jar-component
           (output-file 'compile-op x))
          (parent-component
           (all-resource-files x))
          (java-resource
           (component-pathname x)))))

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
  #+nil(log:info "compiling: ~s" s)
  (destructuring-bind (jar dir) (output-files o s)
    (when (uiop:directory-exists-p dir)
      (uiop:delete-directory-tree dir :validate (lambda (x)
                                                  (or
                                                   (equal nil (pathname-name x))
                                                   (equal "class" (pathname-type x))))))
    (ensure-directories-exist dir)
    (cond
      ((all-java-files s)
       (loop for x in (compile-class-path s)
             do
                (assert (uiop:file-exists-p x)))
       (let ((cmd `("javac"
                    "-d" ,(namestring dir)
                    "-cp" ,(join ":" (mapcar 'namestring (compile-class-path s)))
                    ,@(mapcar 'namestring (all-java-files s)))))
         (safe-run-program cmd))))

    (copy-resources s dir)
    (when (path:-e jar)
     (delete-file jar))
    (safe-run-program (list "jar"
                            "cf" (namestring jar)
                            "-C" (namestring dir)
                            "."))))


(defmethod component-depends-on ((o compile-op) (s java-library))
  `((load-op ,@(system-depends-on s))
    ,@ (call-next-method)))

(defmethod copy-resources ((s asdf:component) directory)
  (loop for x in (component-children s) do
    (etypecase x
      (parent-component
       (copy-resources x  (uiop:merge-pathnames*
                           (make-pathname :directory (list :relative (component-name s)))
                           directory)))
      (jar-resource
       (let ((dest (uiop:merge-pathnames*
                    (component-relative-pathname x)
                    directory)))
         (ensure-directories-exist dest)
         (copy-file (component-pathname x)
                    dest)))
      (t
       (values)))))

(defclass jar-bundle-op (selfward-operation)
  ((asdf:selfward-operation :initform '(compile-op)
                            :allocation :class))
  (:documentation "Creates a bundled jar with all the runtime dependencies"))

(defmethod output-files ((o jar-bundle-op) (lib java-library))
  (list
   (format nil "~a.bundle.jar" (component-name lib))))

(defmethod runtime-depends-on ((lib java-library))
  (let ((compile-deps (mapcar #'asdf:coerce-name (compile-depends-on lib)))
        (all-deps (mapcar #'asdf:coerce-name (system-depends-on lib))))
    (set-difference all-deps compile-deps :test 'string-equal)))

(defmethod runtime-depends-on (lib)
  (system-depends-on lib))

(defmethod collect-runtime-jars ((lib provides-jar))
  (list
   (jar lib)))

(defmethod collect-runtime-jars ((lib java-library))
  (log:info "Collect jars for: ~s" lib)
  (append
   (loop for dep in (runtime-depends-on lib)
      appending
        (collect-runtime-jars (find-system dep)))
   (loop for dep in (component-children lib)
      appending
	(collect-runtime-jars dep))
   (call-next-method)))

(defmethod collect-runtime-jars (lib)
  nil)

(defmethod collect-runtime-jars :around (lib)
  (remove-duplicates (call-next-method) :test 'equal))


(defun merge-jars (output input)
  (tmpdir:with-tmpdir (x)
    (safe-run-program
     (list "unzip"
           (namestring input)
           "-d" x))
    (safe-run-program
     (list "ls" x))
    (safe-run-program
     (list "jar"
           "-uf" output
           "-C" x
           "."))))

(defmethod perform ((o jar-bundle-op) (lib java-library))
  #+nil(log:info "bundling: ~s" lib)
  (let ((output-file (output-file o lib))
        (jars (collect-runtime-jars lib)))
    (uiop:with-staging-pathname (output-file)
      (uiop:copy-file (car jars) output-file)
      (loop for x in (cdr jars) do
        (merge-jars output-file x)))))

(defmethod asdf:perform ((o compile-op) (s java-file))
  ;; do nothing! All compilation is on the system level
  nil)

(defmethod asdf:perform ((o load-op) (s java-file))
  nil)

;; (all-java-files (find-system :java.main))
;; (compile-system :java.main)
;; (compile-class-path (find-system :clinjy/java))
