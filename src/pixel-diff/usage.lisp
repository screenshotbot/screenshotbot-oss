(defpackage :pixel-diff/usage
  (:use #:cl
        #:capi)
  (:export #:show-usage-dialog))
(in-package :pixel-diff/usage)

(defun show-usage-dialog ()
  "Display a dialog showing usage instructions for pixel-diff"
  (format t "Usage: pixel-diff <image1> <image2>~%")
  (format t "       pixel-diff <ref1> <ref2>~%")
  (format t "       pixel-diff <ref1>~%")
  (format t "  Compare two images, git references, or git ref vs working directory~%")
  (let ((dialog (make-instance 'capi:interface
                               :title "Pixel Diff - Usage"
                               :layout (make-instance 'capi:column-layout
                                                      :description
                                                      (list
                                                       (make-instance 'capi:display-pane
                                                                      :text "Pixel Diff Usage"
                                                                      :font (gp:make-font-description
                                                                             :size 16
                                                                             :weight :bold)
                                                                      :visible-min-width 400
                                                                      :visible-min-height 40)
                                                       (make-instance 'capi:display-pane
                                                                      :text "pixel-diff <image1> <image2>   - Compare two image files"
                                                                      :visible-min-width 400
                                                                      :visible-min-height 30)
                                                       (make-instance 'capi:display-pane
                                                                      :text "pixel-diff <ref1> <ref2>     - Compare two git references"
                                                                      :visible-min-width 400
                                                                      :visible-min-height 30)
                                                       (make-instance 'capi:display-pane
                                                                      :text "pixel-diff <ref1>            - Compare git ref vs working directory"
                                                                      :visible-min-width 400
                                                                      :visible-min-height 30)
                                                       (make-instance 'capi:row-layout
                                                                      :description
                                                                      (list
                                                                       nil
                                                                       (make-instance 'capi:push-button
                                                                                      :text "OK"
                                                                                      :callback (lambda (data interface)
                                                                                                  (declare (ignore data))
                                                                                                  (capi:quit-interface interface))
                                                                                      :callback-type :data-interface)
                                                                       nil))))
                               :visible-min-width 450
                               :visible-min-height 250)))
    (capi:display-dialog dialog)))
