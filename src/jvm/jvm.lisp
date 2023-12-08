;; Copyright 2018-Present Modern Interpreters Inc.
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :jvm
  (:use :cl
   :alexandria)
  #+lispworks
  (:import-from #:deliver-utils/common
                #:guess-root)
  (:import-from #:util/misc
                #:relpath)
  (:import-from #:util/health-check
                #:def-health-check)
  (:export :jvm-init
           :*libjvm*
           :free-memory
           :total-memory))
(in-package :jvm)

#+lispworks
(eval-when (:compile-toplevel  :load-toplevel)
  (require "java-interface"))

(defun root ()
  (cond
    #+lispworks
    ((hcl:delivered-image-p)
     (guess-root))
    (t
     (truename
      (path:catdir
       (asdf:system-source-directory :java.main)
       "../../../")))))

(defvar *classpath-relative*
  (let ((root (root)))
   (loop for path in (build-utils:java-class-path
                      (asdf:find-system :java.main))
         collect
         (relpath path root))))

(defun jvm-get-classpath ()
  (let ((class-path
          (let ((root (root)))
            (loop for path in *classpath-relative*
                  collect (path:catfile root path)))))
    #+ccl
    (pushnew (asdf:system-relative-pathname :cl+j "cl_j.jar")
             class-path)

    #+lispworks
    (pushnew
     (relpath (asdf:output-file 'asdf:compile-op (asdf:find-system :jvm/lispcalls)) (root))
     class-path)
    (log:info "Using classpath: ~S" class-path)

    class-path))

(defvar *libjvm* nil
  "Configure with --libjvm")

(defun libjvm.so ()
  (or
   *libjvm*
   (cond
    ((uiop:os-windows-p)
     (let* ((openjdk #P"C:/Program Files/OpenJDK/")
            (versions (fad:list-directory openjdk)))
       (loop for version in versions
             for jvm = (path:catfile version "bin/server/jvm.dll")
               if (path:-e jvm)
               do (return jvm)
             finally
               (error "Could not find java, pass '--libjvm' argument"))))

    (t
     (let* ((platform #+arm64 "arm64"
                      #-arm64 "amd64")
            (guesses (list
                      ;; Debian bookworm
                      (format nil "/usr/lib/jvm/java-17-openjdk-~a/lib/server/libjvm.so" platform)
                      ;; Debian Bullseye
                      (format nil "/usr/lib/jvm/java-11-openjdk-~a/lib/server/libjvm.so" platform)
                      "/usr/lib/jvm/java-11-openjdk/lib/server/libjvm.so"
                      "/opt/homebrew/opt/openjdk/libexec/openjdk.jdk/Contents/Home/lib/server/libjvm.dylib")))
       (loop for guess in guesses
             if (path:-e guess)
               do (return guess)
             finally
               (error "Could not find java or libjvm.so, pass --libjvm argument")))))))

#+ccl
(defun jvm-init-for-ccl ()
  (setf cl-user::*jvm-path* (libjvm.so))
  (setf cl-user::*jvm-options*
        (list "-Xrs"
              (format nil "-Djava.class.path=~a"
                      (str:join ":" (mapcar 'namestring (jvm-get-classpath)))))))

(defun jvm-init ()
  #+lispworks
  (lw-ji:init-java-interface
   :java-class-path (jvm-get-classpath)
   :option-strings (list #+nil"-verbose")
   :jvm-library-path (libjvm.so))

  #+ccl
  (progn
    (jvm-init-for-ccl)
    (pushnew "local-projects/cl+j-0.4/" asdf:*central-registry*)
    (asdf:load-system :cl+j)
    (funcall (find-symbol "JAVA-INIT" "CL+J")))

  #+(and :lispworks (not :screenshotbot-oss))
  (lw-ji:find-java-class "io.tdrhq.TdrhqS3"))

#+lispworks
(lw-ji:define-java-callers "java.lang.Runtime"
  (get-runtime "getRuntime")
  (%total-memory "totalMemory")
  (%free-memory "freeMemory"))

(defun total-memory ()
  #+lispworks
  (%total-memory (get-runtime)))

(defun free-memory ()
  #+lispworks
  (%free-memory (get-runtime)))

#+lispworks
(def-health-check lispcalls.jar-is-accessible ()
  (lw-ji:find-java-class "com.lispworks.LispCalls"))
