(defpackage :pixel-diff/browser
  (:use #:cl
        #:capi)
  (:import-from #:pixel-diff/differ
                #:set-image-pair
                #:open-menu-available-p
                #:comparison-image-layer
                #:image-main-layout
                #:image-pane
                #:image-window
                #:image-layer)
  (:local-nicknames (#:image-pair #:pixel-diff/image-pair)))
(in-package :pixel-diff/browser)

(define-interface image-browser-window (image-window)
  ((image-pair-list :initarg :image-pair-list :initform nil
                    :accessor image-pair-list)
   (current-index :initform 0
                  :accessor current-index))
  (:panes)
  (:layouts
   (main-layout
    row-layout
    '(image-selector :divider image-main-layout)
    :x-ratios `(1 nil 8))
   (image-selector
    column-layout
    '()
    :vertical-scroll t
    :x-adjust :left
    :reader image-selector))
  (:default-initargs
   :create-callback '%create-callback
   :title "Image Browser"))

(defclass left-image-pane (image-pane)
  ((num :initarg :num
        :documentation "only for debugging")))

(defun %create-callback (interface)
  (log:info "here")
  (let ((selector (image-selector interface)))
    (setf (capi:layout-description selector)
          (loop for image-pair in (image-pair-list interface)
                for i from 0
                collect
                (let ((image-pair image-pair))
                  (make-instance 'left-image-pane
                                 :num i
                                 :visible-min-height 100
                                 :visible-max-height 200
                                 :visible-border nil
                                 :image1 (make-instance 'image-layer
                                                        :image (image-pair:updated image-pair)
                                                        :alpha 1)
                                 :image2 nil
                                 :input-model `(((:button-1 :release)
                                                 ,(lambda (image-pane x y)
                                                    (image-selector-callback image-pane x y image-pair))))))))))


(defun image-selector-callback (image-pane x y image-pair)
  (declare (ignore x y))
  (set-image-pair
   (image-pane (capi:element-interface image-pane))
   image-pair))

(defmethod open-menu-available-p ((self image-browser-window))
  nil)

(defun image-list-item-name (image-pair)
  "Returns a display name for an image pair in the list selector"
  (format nil "~A vs ~A" 
          (file-namestring (pixel-diff/image-pair:previous image-pair))
          (file-namestring (pixel-diff/image-pair:updated image-pair))))

(defun image-selector-callback (interface data)
  "Callback function for image selector list panel"
  (declare (ignore data))
  #+nil
  (when (image-pair-list interface)
    (let ((selected-index (choice-selected-item (image-selector interface))))
      (when selected-index
        (setf (current-index interface) selected-index)
        (update-image-display interface)))))

(defun update-image-display (interface)
  "Updates the image display with the currently selected image pair"
  #+nil
  (when (and (image-pair-list interface)
             (< (current-index interface) (length (image-pair-list interface))))
    (let ((current-pair (nth (current-index interface) (image-pair-list interface))))
      ;; Update image display logic would go here
      )))

(defun test-browser-window ()
  "Create a test browser window with three image pairs using the same example images"
  (let ((image-pairs (list
                      (pixel-diff/image-pair:make-image-pair
                       "src/pixel-diff/examples/image1.png"
                       "src/pixel-diff/examples/image2.png")
                      (pixel-diff/image-pair:make-image-pair
                       "src/pixel-diff/examples/image1.png"
                       "src/pixel-diff/examples/image2.png")
                      (pixel-diff/image-pair:make-image-pair
                       "src/pixel-diff/examples/image1.png"
                       "src/pixel-diff/examples/image2.png"))))
    (capi:contain
     (let ((image-layer1 (make-instance 'image-layer
                                        :image "src/pixel-diff/examples/image1.png"
                                        :alpha 0.1))
           (image-layer2 (make-instance 'image-layer
                                        :image "src/pixel-diff/examples/image2.png"
                                        :alpha 1)))
       (make-instance 'image-browser-window
                      :image1 image-layer1
                      :image2 image-layer2
                      :image-pair-list image-pairs)))))



;; (Test-browser-window)
