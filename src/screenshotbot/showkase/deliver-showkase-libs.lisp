(defpackage :screenshotbot/showkase/deliver-showkase-libs
  (:use #:cl
        #:lw))
(in-package :screenshotbot/showkase/deliver-showkase-libs)

(format t "Features: ~S" *features*)

(load "scripts/prepare-image.lisp")
(load "scripts/init.lisp")

(ql:quickload :screenshotbot.showkase)

(defvar *release-build* nil)
(defvar *project-path*
  (merge-pathnames #P"src/screenshotbot/showkase/instr/" (hcl:get-working-directory))
  "Points to the directory where the OthelloDemo project is")

(hcl:deliver-to-android-project 'screenshotbot/showkase/main:main
                                *project-path*
                                (if *release-build* 3 0)
                                :keep-symbols nil
                                :library-name "sbshowkase"
                                :keep-macros t
                                :keep-pretty-printer t
                                :keep-debug-mode (unless *release-build* :all))
