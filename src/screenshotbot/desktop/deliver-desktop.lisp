(ql:quickload "screenshotbot.desktop")
(ql:quickload :deliver-utils)

(uiop:call-function "screenshotbot/desktop/pre-deliver:call-pre-delivery")

(deliver-utils/common:deliver-common
 (ensure-directories-exist
  (car (asdf:output-files
        'asdf:compile-op
        (asdf:find-component
         :screenshotbot.desktop/deliver
         "deliver-desktop"))))
 :restart-fn #'screenshotbot/desktop/init:main
 :prepare-asdf nil
 :require-modules nil
 :keep-clos-object-printing t
 :keep-lisp-reader t
 :keep-modules nil
 :keep-pretty-printer t
 :keep-function-name t
 :keep-clos :meta-object-slots
 :deliver-level 5)
