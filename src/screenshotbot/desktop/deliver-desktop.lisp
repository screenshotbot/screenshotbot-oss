(ql:quickload "screenshotbot.desktop")
(ql:quickload :deliver-utils)

(deliver-utils/common:deliver-common
 (ensure-directories-exist
  (car (asdf:output-files
        'asdf:compile-op
        (asdf:find-component
         :screenshotbot.desktop/deliver
         "deliver-desktop"))))
 :restart-fn #'screenshotbot/desktop/init:main
 :require-modules nil
 :keep-clos-object-printing t
 :keep-lisp-reader t
 :keep-modules nil
 :keep-function-name t
 :keep-clos :meta-object-slots
 :deliver-level 5)
