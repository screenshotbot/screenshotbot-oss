(in-package :cl-user)

(ql:quickload :screenshotbot.sdk)

(defun deliver-main ()
 (let ((output-file (cadddr system:*line-arguments-list*)))
   (delete-file output-file)
   (deliver 'screenshotbot-sdk:main output-file 5
            :keep-function-name t
            :keep-pretty-printer t
            :keep-lisp-reader t
            :keep-symbols `(system:pipe-exit-status)
            :packages-to-keep-symbol-names :all
            :multiprocessing t)))

(deliver-main)
(uiop:quit)
