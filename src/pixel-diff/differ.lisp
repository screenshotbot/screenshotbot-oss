(defpackage :pixel-diff/differ
  (:use #:cl
        #:capi)
  (:import-from #:screenshotbot-js
                #:animate-transform))
(in-package :pixel-diff/differ)

(defun create-empty-pane ()
  (make-instance 'output-pane
                 :background :white
                 :width 400
                 :height 300))

(defun draw-image-layer (pane image-layer  x y width height)
  (declare (ignore x y width height))

  (when (> (alpha image-layer) 0)
    (let ((image (read-image pane image-layer)))
      (when (< (alpha image-layer) 0.9) ;; not fully opaque
        (gp:draw-rectangle pane 0 0 (gp:image-width image) (gp:image-height image)
                          :filled t
                          :foreground :white))
      (when image
        (gp:draw-image pane image 0 0 :global-alpha (alpha image-layer))))))


(defun draw-checkerboard-background (pane x y width height)
  "Draw a checkerboard pattern background to show transparency"
  (let ((square-size 20)
        (light-color :gray95)
        (dark-color :gray85))
    (loop for row from 0 below (ceiling height square-size) do
      (loop for col from 0 below (ceiling width square-size) do
        (let ((rect-x (+ x (* col square-size)))
              (rect-y (+ y (* row square-size)))
              (rect-width (min square-size (- width (* col square-size))))
              (rect-height (min square-size (- height (* row square-size)))))
          (gp:draw-rectangle pane rect-x rect-y rect-width rect-height
                             :filled t
                             :foreground (if (evenp (+ row col))
                                           light-color
                                           dark-color)))))))



(defun draw-image-callback (pane x y width height)
  "Callback function to draw the image in the display pane"
  (let* ((interface (capi:element-interface pane)))
    (draw-checkerboard-background pane x y width height)
    (maybe-init-core-transform interface pane (gp:port-width pane) (gp:port-height pane))
    (log:debug "Transform is ~a" (core-transform interface))
    (assert (core-transform interface))
    (assert (image-transform interface))
    (let ((transform (gp:copy-transform (core-transform interface))))
      (gp:postmultiply-transforms
       transform
       (image-transform interface))
      (gp:with-graphics-transform (pane transform)
        (draw-image-layer pane (image1 interface) x y width height)
        (draw-image-layer pane (image2 interface) x y width height)
        (draw-image-layer pane (comparison interface) x y width height)))))

(defun maybe-init-core-transform (interface pane width height)
  (unless (and
           (core-transform interface)
           (eql width (last-width interface))
           (eql height (last-height interface)))
    (let ((image (gp:load-image pane (image (image1 interface)))))
      (let ((screenshotbot-js-stubs::*make-matrix-impl* #'gp:make-transform))
        (setf (last-width interface) width)
        (setf (last-height interface) height)
        (setf (core-transform interface)
              (screenshotbot-js::calc-core-transform
               width
               height
               (gp:image-width image)
               (gp:image-height image)))))))

(defclass image-layer ()
  ((image :initarg :image
          :reader image)
   (cached-image :initform nil
                 :accessor cached-image)
   (alpha :initarg :alpha
          :accessor alpha)
   (name :initarg :name
         :reader image-layer-name)))

(defmethod read-image (pane (self image-layer))
  (util/misc:or-setf
   (cached-image self)
   (gp:load-image pane (image self))))

(defclass image-pane (output-pane)
  ((press-start :initform nil
                :accessor press-start
                :documentation "The coordinates of an initial press start")))

(define-interface image-window ()
  ((image1 :initarg :image1 :initform nil
           :reader image1)
   (image2 :initarg :image2 :initform nil
           :reader image2)
   (comparison :initarg :comparison :initform nil
               :reader comparison)
   (image-transform :initform (gp:make-transform 1 0 0 1 0 0)
                    :accessor image-transform
                    :documentation "The transform for the image")
   (core-transform :initform nil
                   :accessor core-transform)
   (last-width :initform nil
               :accessor last-width)
   (last-height :initform nil
                :accessor last-height))
  (:panes
   (image-pane image-pane
               :reader image-pane
               :display-callback 'draw-image-callback
               :background :white
               :visible-min-width 400
               :visible-min-height 300
               :input-model `(((:button-1 :press)
                               image-pane-press)
                              ((:button-1 :release)
                               image-pane-release)
                              ((:motion :button-1)
                               image-pane-drag)
                              ((:motion)
                               image-pane-mouse-move)
                              (:character
                               image-pane-char-press)))
   (view-radio-panel capi:radio-button-panel
                     :reader view-radio-panel
                     :items '(:previous :diff :updated)
                     :print-function (lambda (item)
                                       (string-capitalize (symbol-name item)))
                     :selected-item :diff
                     :layout-class 'row-layout
                     :callback-type :data-interface
                     :selection-callback 'view-radio-panel-callback)
   (zoom-button push-button
                :reader zoom-button
                :text "Zoom to change"
                :callback 'zoom-to-change-callback)
   (status-text display-pane
                :reader status-text
                :text "Ready - Move mouse over image to see pixel info"))
  (:layouts
   (main-layout
    column-layout
    '(image-pane bottom-bar status-text))
   (bottom-bar
    row-layout
    '(view-radio-panel nil  zoom-button)))
  (:menus
   (view-menu "View"
              (("Toggle Previous/Updated" :data :toggle-previous-updated
                                          :callback-type :interface
                                          :callback #'toggle-previous-updated
                                          :accelerator "v")
               ("Zoom In" :data :zoom-in
                          :callback-type :interface
                          :callback #'zoom-in-callback
                          :accelerator "+")
               ("Zoom Out" :data :zoom-out
                           :callback-type :interface
                           :callback #'zoom-out-callback
                           :accelerator "-"))))
  (:menu-bar view-menu)
  (:default-initargs
   :title "Image Display Window"
   :width 450
   :height 350))

(defun view-radio-panel-callback (item interface)
  "Callback function for view radio panel selection changes"
  (log:info "View changed to: ~a" item)
  (case item
    (:previous
     (setf (alpha (image1 interface)) 1.0)
     (setf (alpha (image2 interface)) 0.0)
     (setf (alpha (comparison interface)) 0.0))
    (:diff
     (setf (alpha (image1 interface)) 0.1)
     (setf (alpha (image2 interface)) 0.0)
     (setf (alpha (comparison interface)) 1.0))
    (:updated
     (setf (alpha (image1 interface)) 0.0)
     (setf (alpha (image2 interface)) 1.0)
     (setf (alpha (comparison interface)) 0.0)))
  (gp:invalidate-rectangle (image-pane interface)))


(defun render-color (color)
  (flet ((%ref (num)
           (floor (* 255
                     (aref color num)))))
    (if (vectorp color)
       (case (length color)
         (4 (format nil "#~2,'0x~2,'0x~2,'0xFF" 
                    (%ref 1) (%ref 2) (%ref 3)))
         (5 (format nil "#~2,'0x~2,'0x~2,'0x~2,'0x" 
                    (%ref 1) (%ref 2) (%ref 3) (%ref 4)))
         (otherwise (format nil "~a" color)))
       (format nil "~a" color))))


(defun get-image-layer-color (pane image-layer image-x image-y)
  "Get the color of a pixel from an image layer at the specified coordinates"
  (when image-layer
    (let ((image (read-image pane image-layer)))
      (when (and image 
                 (< image-x (gp:image-width image))
                 (< image-y (gp:image-height image)))
        (let ((image-access (gp:make-image-access pane image)))
          (unwind-protect
               (gp:image-access-pixel image-access image-x image-y)
            (gp:free-image-access image-access)))))))

(defun image-pane-mouse-move (pane x y)
  "Handle mouse movement over image pane to show pixel information"
  (let ((interface (capi:element-interface pane)))
    (when (and (core-transform interface) (image-transform interface))
      (let ((combined-transform (gp:copy-transform (core-transform interface))))
        (gp:postmultiply-transforms combined-transform (image-transform interface))
        (multiple-value-bind (image-x image-y)
            (gp:transform-point (gp:invert-transform combined-transform) x y)
          (let ((image-x (round image-x))
                (image-y (round image-y)))
            (when (and (>= image-x 0) (>= image-y 0))
              (let* ((color-before (get-image-layer-color pane (image1 interface) image-x image-y))
                     (color-after (get-image-layer-color pane (image2 interface) image-x image-y)))
                (setf
                 (capi:display-pane-text (status-text interface))
                 (cond
                   ((and color-before color-after
                         (equalp color-before color-after))
                    (format nil "Identitical color: ~a" (render-color color-before)))
                   ((and color-before color-after)
                    (format nil "Color changed from ~a to ~a" (render-color color-before) (render-color color-after)))
                   (color-before
                    (format nil "Color was ~a, now out of bounds" (render-color color-before)))
                   (color-after
                    (format nil "Out of bounds earlier, not color is: ~a" (render-color color-after) ))
                   (t
                    "Ready - Move mouse over image to see pixel info")))))))))))



(defun start-animation-timer (pane duration-seconds callback
                              &key (finally (lambda ())))
  "Start an animation timer that calls callback with progress from 0.0 to 1.0"
  (let ((start-time (get-internal-real-time))
        (duration-internal (* duration-seconds internal-time-units-per-second))
        (count-cons (list 1)))
    (labels ((timer-tick (count-cons)
               (let* ((current-time (get-internal-real-time))
                      (elapsed (- current-time start-time))
                      (progress (min 1.0 (/ elapsed duration-internal))))
                 (when (> (car count-cons) 0)
                   (capi:apply-in-pane-process-if-alive
                    pane
                    (lambda ()
                      (funcall callback progress)))
                   (cond
                     ((< progress 1.0)
                      (values))
                     ((>= progress 1.0)
                      (setf (car count-cons) -1)
                      (capi:apply-in-pane-process-if-alive
                       pane
                       finally)
                      :stop))))))
      (let ((timer (mp:make-timer #'timer-tick count-cons)))
        (mp:schedule-timer-relative-milliseconds timer 32 32)))))



(defun %zoom-to (interface x y &key (zoom 5) (finally (lambda ())))
  (let ((pane (slot-value interface 'image-pane)))
    (let* ((image (gp:load-image pane (image (image1 interface))))
           (start-mat (transform-to-3dmat (image-transform interface)))
           (final-mat (screenshotbot-js::calc-transform-for-center
                       (gp:port-width pane)
                       (gp:port-height pane)
                      (gp:image-width image)
                      (gp:image-height image)
                      x y zoom)))
      (start-animation-timer
       (image-pane interface)
       2
       (lambda (progress)
         (setf (image-transform interface)
               (3dmat-to-transform
                (animate-transform start-mat final-mat progress)))
         (gp:invalidate-rectangle pane))
       :finally finally))))


(defmethod find-non-transparent-pixel (pane (image gp:image))
  (let ((image-access (gp:make-image-access pane image)))
    (unwind-protect
         (let ((width (gp:image-width image))
               (height (gp:image-height image)))
           (loop for y from 0 below height do
             (loop for x from 0 below width do
               (let ((color (gp:image-access-pixel image-access x y)))
                 (when (= 4 (length color)) ;; #(:RGB r g b a) with alpha, and #(:RBG r g b) without alpha.
                   (hcl:gc-generation t)
                   (return-from find-non-transparent-pixel (values x y))))))
           nil)
      (gp:free-image-access image-access))))

(defun zoom-to-change-callback (data interface)
  "Callback function for zoom-to-change button - currently just logs"
  (declare (ignore data))
  (multiple-value-bind (x y)
      (find-non-transparent-pixel
       (image-pane interface)
       (read-image (image-pane interface)
                   (comparison interface)))
    (setf (capi:button-enabled (zoom-button interface))  nil)
    (%zoom-to interface x y
              :finally
              (lambda ()
                (setf (capi:button-enabled (zoom-button interface)) t))))
  (log:info "Zoom to change button pressed for interface: ~a" interface))

(defun zoom-in-callback (interface)
  "Callback function for zoom in menu item"
  (let ((pane (image-pane interface)))
    (let ((center-x (/ (gp:port-width pane) 2))
          (center-y (/ (gp:port-height pane) 2)))
      (process-zoom pane center-x center-y 1.2))))

(defun zoom-out-callback (interface)
  "Callback function for zoom out menu item"
  (let ((pane (image-pane interface)))
    (let ((center-x (/ (gp:port-width pane) 2))
          (center-y (/ (gp:port-height pane) 2)))
      (process-zoom pane center-x center-y 0.8))))



(defun transform-to-3dmat (transform)
  (destructuring-bind (a b c d e f) transform
    (3d-matrices:mat3 (vector a c e
                              b d f
                              0 0 1))))

(defun 3dmat-to-transform (mat3)
  "Convert a 3d-matrices:mat3 matrix to a graphics port transform"
  (let ((m (3d-matrices:marr mat3)))
    (gp:make-transform (aref m 0) (aref m 3) (aref m 1) (aref m 4) (aref m 2) (aref m 5))))



(defun process-zoom (pane x y delta)
  (let ((interface (capi:element-interface pane)))
    (let ((screenshotbot-js-stubs::*make-matrix-impl* #'gp:make-transform))
      (let ((dm (screenshotbot-js::calc-transform-for-zoom x y
                                                           (transform-to-3dmat
                                                            (image-transform interface))
                                                           delta)))
        (gp:postmultiply-transforms
         (image-transform interface)
         dm)
        (gp:invalidate-rectangle pane)))))

(defun toggle-previous-updated (interface)
  "Toggle between showing the previous image and the updated image"
  (let* ((current-selection (capi:choice-selected-item (view-radio-panel interface))))
    (case current-selection
      (:previous
       (setf (capi:choice-selected-item (view-radio-panel interface)) :updated)
       (view-radio-panel-callback :updated interface))
      (otherwise
       (setf (capi:choice-selected-item (view-radio-panel interface)) :previous)
       (view-radio-panel-callback :previous interface)))))

(defun image-pane-char-press (pane x y character)
  (log:info "Got ~a for ~a,~a" character x y)
  (case character
    (#\+
     (process-zoom pane x y 1.3))
    (#\-
     (process-zoom pane x y (/ 1 1.3)))))

(defun image-pane-press (pane x y)
  "Handle mouse button press on image pane"
  (setf (press-start pane)
        (cons x y))
  (log:info "Image pane press at (~a, ~a)" x y))

(defun image-pane-release (pane x y)
  "Handle mouse button release on image pane"
  (setf (press-start pane) nil)
  (log:info "Image pane release at (~a, ~a)" x y))

(defun image-pane-drag (pane x y)
  "Handle mouse drag on image pane"
  (log:info "Image pane drag at (~a, ~a)" x y)
  (when (press-start pane)
    (destructuring-bind (startx . starty)
        (press-start pane)
      (let ((interface (capi:element-interface pane)))
        (gp:postmultiply-transforms
         (image-transform interface)
         (gp:make-transform 1 0 0 1 (- x startx) (- y starty)))
        (setf (press-start pane)
              (cons x y)))
      (gp:invalidate-rectangle pane))))



(defun create-empty-interface (&key image1 image2)
  (uiop:with-temporary-file (:pathname comparison :keep t :type "png")
    (uiop:run-program
     (list "magick" "compare"
           "-compose" "src"
           "-define" "compare:lowlight-color=none"
           "-define" "compare:highlight-color=red"
           (namestring image1)
           (namestring image2)
           (namestring comparison))
     :ignore-error-status t)
    (make-instance 'image-window
                   :title "Empty Interface"
                   :image1 (make-instance 'image-layer
                                          :image image1
                                          :alpha 0.1)
                   :image2 (make-instance 'image-layer
                                          :image image2
                                          :alpha 0)
                   :comparison (make-instance 'image-layer
                                              :image comparison
                                              :alpha 1)
                   :width 400
                   :height 300)))

(defun open-interface (image))


(defun test-example ()
  (display (create-empty-interface
            :image1 "/home/arnold/builds/fast-example/screenshots/image.png"
            :image2 "/home/arnold/builds/fast-example/screenshots-copy/image.png")))

;; (test-example)


