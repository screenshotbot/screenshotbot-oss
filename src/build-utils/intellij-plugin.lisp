(defpackage :build-utils/intellij-plugin
  (:use #:cl
        #:asdf
        #:build-utils/jar-file)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:build-plugin-op))
(in-package :build-utils/intellij-plugin)

(defclass build-plugin-op (selfward-operation)
  ((asdf:selfward-operation :initform '(compile-op)
                            :allocation :class))
  (:documentation "Bundles an IntelliJ plugin. Mostly puts all the
   runtime deps in lib/"))

(defmethod output-files ((o build-plugin-op) (lib java-library))
  (list
   (pathname
    (format nil "~a.ij.jar" (component-name lib)))))


(defmethod asdf:perform ((o build-plugin-op) (lib java-library))
  (tmpdir:with-tmpdir (tmpdir)
   (let* ((jar (car (output-files 'compile-op lib)))
          (runtime-jars (collect-runtime-jars lib))
          (runtime-jars (remove jar runtime-jars :test 'equal))
          (output-file (output-file o lib))
          (lib-dir (uiop:merge-pathnames* #P "lib/"
                                         tmpdir)))
     (ensure-directories-exist lib-dir)
     (uiop:with-staging-pathname (output-file)
       (uiop:copy-file jar output-file)


       (dolist (dep runtime-jars)
         (uiop:copy-file dep (make-pathname
                              :name (pathname-name dep)
                              :type (pathname-type dep)
                              :defaults lib-dir)))

       (safe-run-program
        (list
         "jar"
         "uf" output-file
         "-C" tmpdir
         "."))))))
