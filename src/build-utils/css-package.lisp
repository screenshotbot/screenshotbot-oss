;; Copyright 2018-Present Modern Interpreters Inc.
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :build-utils/css-package
  (:use :cl
        :asdf
   :alexandria)
  (:import-from :build-utils/js-package
                :web-asset)
  (:export :css-library
   :css-system
           :css-file
   :scss-file))
(in-package :build-utils/css-package)

(defun sanitize-name (x)
  (substitute #\~ #\/ x))

(defclass copy-sources-op (asdf:operation)
  ()
  (:documentation "Copy all the source files to a given output directory"))

(defclass css-library (asdf:system)
  ((import-path :initarg :import-path
                :initform #P ""
                         :accessor import-path)))

(defclass css-system (css-library web-asset)
  ())

(defclass base-css-file (asdf:static-file)
  ())

(defclass scss-file (base-css-file)
  ()
  (:default-initargs :type "scss"))

(defclass css-file (base-css-file)
  ()
  (:default-initargs :type "css"))

(defmethod asdf:input-files ((o copy-sources-op) (c css-library))
  (mapcar
   'asdf:component-pathname
    (asdf:required-components c
                              :keep-component 'base-css-file)))

(defmethod asdf:output-files ((o copy-sources-op) (c css-library))
  (list
   (make-pathname
    :directory `(:relative ,(format nil "~a--sources-copy" (sanitize-name (component-name c)))))))

(defun rel-path (child root)
  (assert child)
  (assert root)
  (cond
    ((eql child root)
     (make-pathname :directory '(:relative)))
    (t
     (let ((parent (asdf:component-parent child)))
       (let ((final
               (merge-pathnames
                (etypecase child
                  (module
                   (asdf:component-relative-pathname child))
                  (t
                   (asdf:component-relative-pathname child)))
                (rel-path parent root))))
         final)))))



(defmethod asdf:perform ((o copy-sources-op) (c css-library))
  #+nil(log:info "performing: ~a, ~a" o c)
  (let ((output-dir (asdf:output-file o c)))
    (uiop:delete-directory-tree output-dir
                                :validate
                                (lambda (x)
                                  (validate-dir-p output-dir x)))
    (loop for component in (asdf:required-components c :keep-component 'base-css-file)
          for rel-path = (rel-path component c)
          if (not (typep component 'asdf:module))
            do
               (let ((output (merge-pathnames
                              (merge-pathnames
                               rel-path
                               (import-path c))
                              output-dir))
                     (input (asdf:component-pathname component)))
                 (assert (pathname-name output))
                 (ensure-directories-exist output)
                 (uiop:copy-file input output)))))


(defmethod asdf:input-files ((o compile-op) (c css-system))
  (call-next-method))



(defmethod output-files ((o compile-op) (c css-system))
  (let ((name (string-downcase (sanitize-name (component-name c)))))
    (list
     (pathname
      (format nil "~a.css" name))
     (pathname
      (format nil "~a.css.map" name)))))

(defun copy-component-children (c output-dir root-dir)
  (loop for x in (component-children c)
        do
           (typecase x
             (asdf:module
              ;;(format t "copying module: ~a (~S)~%" x (component-children x))
              (copy-component-children
               x
               output-dir
               root-dir))
             (t
              (let* ((pathname (component-pathname x))
                     (rel-dir (subseq (pathname-directory pathname)
                                      (length (pathname-directory root-dir))))
                     (output-file (make-pathname
                                   :name (pathname-name (component-pathname x))
                                   :type (asdf:file-type x)
                                   :directory (append (pathname-directory output-dir) rel-dir)
                                   :defaults output-dir)))
                (ensure-directories-exist output-file)
                (uiop:copy-file (component-pathname x)
                                output-file))))))


(defun validate-dir-p (output-dir x)
  (let ((prefix (pathname-directory output-dir)))
    (equal
     prefix
     (subseq (pathname-directory x)
             0 (length prefix)))))

(defun sass ()
  (flet ((rel (name)
           (namestring
            (asdf:system-relative-pathname :build-utils name))))
   (cond
     ((uiop:os-macosx-p)
      (rel "darwin/dart-sass/sass"))
     ((uiop:os-unix-p)
      (or
       #+arm64
       (rel "linux-arm64/dart-sass/sass")
       (rel "dart-sass/sass")))
     ((uiop:os-windows-p)
      ;; choco install sass
      "sass")
     (t
      (error "unsupported implementation could not find `sass`")))))



(defmethod asdf:component-depends-on ((o copy-sources-op) (c css-library))
  (loop for x in (asdf:system-depends-on c)
        collect `(copy-sources-op ,x)))

(defmethod asdf:component-depends-on ((o compile-op) (c css-system))
  `((copy-sources-op ,(asdf:component-name c))))

(defmethod asdf:perform ((o compile-op) (c css-system))
  (destructuring-bind
      (output-file source-map)
      (output-files o c)
    (let ((tmp-output (make-pathname :type "css-tmp" :defaults output-file)))
      (multiple-value-bind (out err ret)
          (uiop:run-program
           `(,(sass)
             "--style=compressed"
             ,@ (loop for dep in (required-components c
                                                      :other-systems t
                                                      :keep-component 'css-library)
                      append (list "-I" (namestring (car (output-files
                                                          'copy-sources-op
                                                           dep)))))
             ,(let ((child (car (input-files 'copy-sources-op c))))
                (namestring
                 child))
             ,(namestring tmp-output))
           :element-type 'character
           :external-format :latin-1
           :output 'string
           :error-output 'string
           :ignore-error-status t)
        (unless (= ret 0)
          (error "Could not compile css assets: ~%~%stdout:~a~%~%stderr:~%~a~%" out err)))
      (uiop:rename-file-overwriting-target tmp-output output-file)
      (uiop:rename-file-overwriting-target (format nil "~a.map" (namestring tmp-output))
                                           source-map))))

(defun get-css-component (dir)
  (cond
    ((path:-d dir)
     (let ((name (car (last (pathname-directory dir)))))
       `(:module ,name :components (,@ (remove-if 'null
                                                   (loop for x in (fad:list-directory dir)
                                                         collect (get-css-component x)))))))
    ((equal "scss" (pathname-type dir))
     `(,(let ((*package* :cl-user)) (format nil "~s" 'scss-file)) ,(pathname-name dir)))
    ((equal "css" (pathname-type dir))
     `(,(let ((*package* :cl-user)) (format nil "~s" 'css-file))
       ,(pathname-name dir)))))
