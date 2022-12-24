(in-package :cl-user)

(ql:quickload :screenshotbot.sdk/gradle)

(defun output-file ()
  (car (asdf:output-files
        'asdf:compile-op
         (asdf:find-component :screenshotbot.sdk/deliver-java "deliver-java"))))

(defun deliver-main ()
  (let ((output-file (output-file)))
    (ensure-directories-exist output-file)

    (deliver 'screenshotbot/sdk/gradle:main
              output-file
              5
              #+mswindows :console #+mswindows :init
              #+mswindows :startup-bitmap-file #+mswindows nil
              :packages-to-keep-symbol-names :all
              :keep-clos-object-printing t
              :keep-symbols `(system:pipe-exit-status
                              simple-test)
              :multiprocessing t)))

(deliver-main)
