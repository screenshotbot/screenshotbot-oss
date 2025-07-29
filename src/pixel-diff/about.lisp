;; -*- coding: utf-8 -*-

(defpackage :pixel-diff/about
  (:use #:cl
        #:capi))
(in-package :pixel-diff/about)

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
                                                                :visible-min-width 300
                                                                :visible-min-height 30)
                                                 (make-instance 'display-pane
                                                                :text "Version 1.0"
                                                                :visible-min-width 300
                                                                :visible-min-height 20)
                                                 (make-instance 'display-pane
                                                                :text "A tool for comparing images and visualizing differences"
                                                                :visible-min-width 300
                                                                :visible-min-height 40)
                                                 (make-instance 'display-pane
                                                                :text "© 2025 Modern Interpreters Inc. (Screenshotbot)"
                                                                :visible-min-width 300
                                                                :visible-min-height 20)
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
                         :visible-min-width 320
                         :visible-min-height 200
                         :owner parent)))
    (capi:display dialog)))
