(defpackage :screenshotbot/client-hub/deliver-client-hub
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria)))
(in-package :screenshotbot/client-hub/deliver-client-hub)

(ql:quickload :screenshotbot.client-hub)

(defun output-file ()
  (car (asdf:output-files 'asdf:compile-op
                          (asdf:find-component
                           :screenshotbot.client-hub/deliver "deliver-client-hub"))))

(defun deliver-main ()
  (let ((output-file (output-file)))
    #-darwin
    (uiop:delete-file-if-exists output-file)
    (lw:deliver 'screenshotbot/client-hub/main:main
             output-file
             5
             :keep-function-name t
             #+mswindows :console #+mswindows :init
             #+mswindows :startup-bitmap-file #+mswindows nil
             :keep-debug-mode t
             :keep-pretty-printer t
             :keep-clos-object-printing t
             :keep-lisp-reader t
             ;; temporary: get the build green
             :keep-eval t
             :keep-symbols `(system:pipe-exit-status)
             :packages-to-keep-symbol-names :all
             :multiprocessing t)))

#-darwin
(deliver-main)

#+darwin
(cond
  ((hcl:building-universal-intermediate-p)
   (deliver-main))
  (t
   (uiop:delete-file-if-exists (output-file))
   (hcl:save-universal-from-script "src/screenshotbot/client-hub/deliver-client-hub.lisp")))

(uiop:quit)
