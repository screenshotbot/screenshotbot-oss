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

(defclass css-library (asdf:system)
  ((import-path :initarg :import-path
                :initform #P ""
                :accessor import-path)))

(defclass css-system (css-library web-asset)
  ())

(defclass scss-file (asdf:static-file)
  ((asdf::type :initform "scss")))

(defclass css-file (asdf:static-file)
  ((asdf::type :initform "css")))


(defmethod output-files ((o compile-op) (c css-system))
  (let ((name (string-downcase (component-name c))))
    (list
     (pathname
      (format nil "~a.css" name))
     (pathname
      (format nil "~a.tmp/" name))
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
                     (output-file (make-pathname :name (pathname-name (component-pathname x))
                                              :type (asdf:file-type x)
                                              :directory (append (pathname-directory output-dir) rel-dir)
                                                 :defaults output-dir)))
                #+nil
                (format t "copying: ~a to ~a~%" (component-pathname x)
                        output-file)
                (ensure-directories-exist output-file)
                (uiop:copy-file (component-pathname x)
                                output-file))))))

(defmethod copy-css-libraries ((c css-library) output-dir)
  (let ((output-dir (path:catdir output-dir (import-path c))))
    (ensure-directories-exist output-dir)
    (loop for x in (system-depends-on c) do
      (copy-css-libraries (find-component x nil) output-dir))
    (copy-component-children c output-dir (component-pathname c))))

(defmethod copy-css-libraries ((c css-system) output-dir)
  (when (probe-file output-dir)
    ;; ideally I should do better than this.
    (uiop:delete-directory-tree output-dir :validate (lambda (x)
                                                       (let ((prefix (pathname-directory output-dir)))
                                                         (equal
                                                          prefix
                                                          (subseq (pathname-directory x)
                                                                  0 (length prefix)))))))
  (call-next-method))

(defmethod asdf:perform ((o compile-op) (c css-system))
  (destructuring-bind
      (output-file copy-dir source-map)
      (output-files o c)
    (declare (ignore source-map))
    (copy-css-libraries c copy-dir)
    (let ((tmp-output (make-pathname :type "css-tmp" :defaults output-file)))
     (uiop:run-program
      (list
       (namestring
        (asdf:system-relative-pathname :build-utils "dart-sass/sass"))
       "-I" (namestring copy-dir)
       (let ((child (car (component-children c))))
         (namestring
          (make-pathname
           :name (component-name
                  child)
           :type (asdf:file-type child)
           :defaults copy-dir)))
       (namestring tmp-output))
      :element-type 'character
      :external-format :latin-1
      :output *standard-output*
      :error-output *error-output*)
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
     `(scss-file ,(pathname-name dir)))
    ((equal "css" (pathname-type dir))
     `(css-file ,(pathname-name dir)))))
