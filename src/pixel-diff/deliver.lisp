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
   "pixel-diff"
   :restart-fn #'pixel-diff/main::main
   :deliver-level 5
   :interface :capi
   :console nil
   :startup-bitmap-file nil
   :keep-modules nil))

;; Run delivery when this file is loaded
(deliver-pixel-diff)

