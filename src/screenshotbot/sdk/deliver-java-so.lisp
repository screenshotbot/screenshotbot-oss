(in-package :cl-user)

(ql:quickload :screenshotbot.sdk)

(defun output-file ()
  (car (asdf:output-files
        'asdf:compile-op
         (asdf:find-component :screenshotbot.sdk/deliver-java-so "deliver-java-so"))))

(defun main ())

(compile 'main)

(defun deliver-main ()
  (let ((output-file (output-file)))
    (ensure-directories-exist output-file)
    (lw-ji:setup-deliver-dynamic-library-for-java)

    (deliver 'main
              output-file
              5
              :keep-function-name t
              #+mswindows :console #+mswindows :init
              #+mswindows :startup-bitmap-file #+mswindows nil
              :image-type :dll
              :keep-clos-object-printing t
              :keep-symbols `(system:pipe-exit-status)
              :multiprocessing t)))

(deliver-main)
