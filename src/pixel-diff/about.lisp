;; -*- coding: utf-8 -*-

(defpackage :pixel-diff/about
  (:use #:cl
        #:capi)
  (:export #:show-about-dialog))
(in-package :pixel-diff/about)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *pixel-diff-version*
    (or (asdf:component-version (asdf:find-system :pixel-diff))
        "Unknown")
    "Version of pixel-diff captured at compile time"))

(defun get-pixel-diff-version ()
  "Get the version of the pixel-diff system"
  *pixel-diff-version*)

(defun show-about-dialog (&optional parent)
  "Create and display an About dialog"
  (let ((dialog
          (make-instance 'interface
                         :title "About Pixel Diff"
                         :layout (make-instance 'column-layout
                                                :description
                                                (list
                                                 (make-instance 'display-pane
                                                                :text "Pixel Diff"
                                                                :font (gp:make-font-description
                                                                       :size 16
                                                                       :weight :bold)
                                                                :visible-min-width 600
                                                                :visible-min-height 60)
                                                 (make-instance 'display-pane
                                                                :text (format nil "Version ~A" (get-pixel-diff-version))
                                                                :visible-min-width 600
                                                                :visible-min-height 40)
                                                 (make-instance 'display-pane
                                                                :text "A tool for comparing images and visualizing differences"
                                                                :visible-min-width 600
                                                                :visible-min-height 80)
                                                 (make-instance 'display-pane
                                                                :text "© 2025 Modern Interpreters Inc. (Screenshotbot)"
                                                                :visible-min-width 600
                                                                :visible-min-height 40)
                                                 (make-instance 'row-layout
                                                                :description
                                                                (list
                                                                 nil
                                                                 (make-instance 'push-button
                                                                                :text "OK"
                                                                                :callback (lambda (data interface)
                                                                                            (declare (ignore data))
                                                                                            (capi:quit-interface interface))
                                                                                :callback-type :data-interface)
                                                                 nil))))
                         :visible-min-width 640
                         :visible-min-height 400
                         :owner parent)))
    (capi:display-dialog dialog)))
