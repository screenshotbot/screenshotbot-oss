;;;; Delivery script for pixel-diff binary

(load "scripts/prepare-image.lisp")
(load "scripts/init.lisp")
(ql:quickload :deliver-utils)
(ql:quickload :pixel-diff)

(defpackage :pixel-diff/deliver
  (:use #:cl)
  (:import-from #:deliver-utils/common
                #:deliver-common))
(in-package :pixel-diff/deliver)

(defun deliver-target ()
   "pixel-diff"
   #+darwin
   (if (str:s-member sys:*line-arguments-list* "--debug")
    "pixel-diff"
    (hcl:create-macos-application-bundle 
     "pixel-diff.app"
     ;; Don't copy LispWorks file associations
     :document-types nil
     :identifier "io.screenshotbot.pixeldiff"
     :application-icns (asdf:system-relative-pathname :pixel-diff "PixelDiff.icns")
     :version (asdf:component-version (asdf:find-system :pixel-diff))
     :bundle-name "Pixel Diff")))


(defun deliver-pixel-diff ()
  "Deliver the pixel-diff binary"
  (deliver-common 
   (deliver-target)
   :restart-fn #'pixel-diff/main::main
   :deliver-level 5
   :interface :capi
   :icon-file (asdf:system-relative-pathname :pixel-diff "logo.ico")
   :warn-on-missing-templates t
   :console :input ;; only if input is attempted
   :startup-bitmap-file nil
   :prepare-asdf nil
   :keep-modules nil))

;; Run delivery when this file is loaded
(deliver-pixel-diff)

