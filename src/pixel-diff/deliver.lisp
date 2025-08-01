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

(defun deliver-pixel-diff ()
  "Deliver the pixel-diff binary"
  (deliver-common 
   #-darwin
   "pixel-diff"
   #+darwin
   (hcl:create-macos-application-bundle 
    "pixel-diff.app"
    ;; Don't copy LispWorks file associations
    :document-types nil
    ;; Set your own bundle identifier
    :identifier "io.screenshotbot.pixel-diff"
    ;; Set version info
    :version "1.0"
    ;; Optionally set bundle name
    :bundle-name "Pixel Diff")
   :restart-fn #'pixel-diff/main::main
   :deliver-level 5
   :interface :capi
   :icon-file (asdf:system-relative-pathname :pixel-diff "logo.ico")
   :warn-on-missing-templates t
   :console :input ;; only if input is attempted
   :startup-bitmap-file nil
   :keep-modules nil))

;; Run delivery when this file is loaded
(deliver-pixel-diff)

