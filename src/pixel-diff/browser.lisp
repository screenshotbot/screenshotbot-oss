(defpackage :pixel-diff/browser
  (:use #:cl
        #:capi)
  (:import-from #:pixel-diff/differ
                #:post-process-image
                #:cached-image
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
                    :accessor image-pair-list))
  (:panes
   (image-list-selector
    list-panel
    :items (image-pair-list interface)
    :print-function 'image-pair:image-pair-title
    :selection-callback 'image-list-selection-callback
    :reader image-list-selector))
  (:layouts
   (main-layout
    row-layout
    '(image-list-selector :divider image-main-layout)
    :x-ratios `(1 nil 8)))
  (:default-initargs
   :create-callback '%create-callback
   :title "Image Browser"))

(defclass left-image-pane (image-pane)
  ((num :initarg :num
        :documentation "only for debugging")))

(defclass thumbnail-image-layer (image-layer)
  ())

(defun calc-scale (image)
  (let ((width (gp:image-width image))
        (height (gp:image-height image)))
    (/ 400 (max width height))))

(defmethod post-process-image (pane (self thumbnail-image-layer) image)
  image)

(defun %create-callback (interface))


(defun image-list-selection-callback (selected-image-pair interface)
  "Callback function for the image list selector panel"
  (set-image-pair
   (image-pane interface)
   selected-image-pair))



(defmethod open-menu-available-p ((self image-browser-window))
  nil)

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
                                        :alpha 0)))
       (make-instance 'image-browser-window
                      :image1 image-layer1
                      :image2 image-layer2
                      :image-pair-list image-pairs)))))



;; (Test-browser-window)
