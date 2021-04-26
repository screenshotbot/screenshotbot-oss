;; Copyright 2018-Present Modern Interpreters Inc.
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(in-package :build-utils)

(defclass web-asset ()
  ())

(defclass js-library (asdf:system)
  ())

(defclass js-system (js-library web-asset)
  ())

(defclass js-file (asdf:static-file)
  ((asdf::type :initform "js")))

(defmethod js-input-files ((o compile-op) (m module))
  (loop for x in (component-children m) appending
        (js-input-files o x)))

(defmethod js-input-files ((o compile-op) (j js-file))
  (asdf:input-files o j))

(defmethod js-input-files ((o compile-op) (j js-library))
  (append
   (loop for x in (system-depends-on j) appending
         (js-input-files o (find-component x nil)))
   (loop for x in (component-children j) appending
                                         (js-input-files o x))))

(defmethod asdf:output-files ((o compile-op) (j js-system))
  (list
   ;; okay, where does this go?
   (format nil "~a.js" (string-downcase (component-name j)))
   (format nil "~a.js.map" (string-downcase (component-name j)))))

(defmethod asdf:perform ((o compile-op) (j js-system))
  (restart-case
      (let ((input-files (remove-duplicates (js-input-files o j) :test 'equal :from-end t)))
        ;;(format *error-output* "Compling: ~S" input-files)
        (loop for x in input-files
              do
                 (unless (probe-file x)
                   (error "The file ~A does not exist" x)))
        (uiop:run-program
         (append
          (list
           "java"
           "-jar"
           (namestring
            (asdf:system-relative-pathname
             :web.all "closure-compiler/closure-compiler-v20200830.jar"))
           ;; "--compilation_level"
           ;; "WHITESPACE_ONLY"
           "--js_output_file"
           (namestring (car (output-files o j)))
           "--create_source_map"
           (namestring (cadr (output-files o j))))
          (mapcar 'namestring
                   input-files))
         :output *standard-output*
         ;;:error-output *error-output*
         )
        (with-open-file (s (car (output-files o j)))
         (format t "File size is: ~A" (file-length s))))
    (retry-perform ()
      (perform o j))))
