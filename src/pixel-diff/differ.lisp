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
  (let ((image (read-image pane image-layer)))
    (when image
      (gp:draw-image pane image 0 0 :global-alpha (alpha image-layer)))))


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
    ;;(draw-checkerboard-background pane x y width height)
    (maybe-init-core-transform interface pane (gp:port-width pane) (gp:port-height pane))
    (log:info "Transform is ~a" (core-transform interface))
    (assert (core-transform interface))
    (assert (image-transform interface))
    (let ((transform (gp:copy-transform (core-transform interface))))
      (gp:postmultiply-transforms
       transform
       (image-transform interface))
      (gp:with-graphics-transform (pane transform)
        (draw-image-layer pane (image1 interface) x y width height)
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
          :reader alpha)
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
                              (:character
                               image-pane-char-press)))
   (zoom-button push-button
                :text "Zoom to change"
                :callback 'zoom-to-change-callback))
  (:layouts
   (main-layout
    column-layout
    '(image-pane bottom-bar))
   (bottom-bar
    row-layout
    '(nil zoom-button)
    :internal-border 10))
  (:default-initargs
   :title "Image Display Window"
   :width 450
   :height 350))

(defun start-animation-timer (duration-seconds callback)
  "Start an animation timer that calls callback with progress from 0.0 to 1.0"
  (let ((start-time (get-internal-real-time))
        (duration-internal (* duration-seconds internal-time-units-per-second))
        (count-cons (list 1)))
    (labels ((timer-tick (count-cons)
               (let* ((current-time (get-internal-real-time))
                      (elapsed (- current-time start-time))
                      (progress (min 1.0 (/ elapsed duration-internal))))
                 (when (> (car count-cons) 0)
                   (funcall callback progress)
                   (when (< progress 1.0)
                     (values))
                   (when (>= progress 1.0)
                     (setf (car count-cons) -1)
                     :stop)))))
      (let ((timer (mp:make-timer #'timer-tick count-cons)))
        (mp:schedule-timer-relative-milliseconds timer 16 16)))))



(defun %zoom-to (interface x y &optional (zoom 5))
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
       1
       (lambda (progress)
         (setf (image-transform interface)
               (3dmat-to-transform
                (animate-transform start-mat final-mat progress)))
      (gp:invalidate-rectangle pane))))))

(defun zoom-to-change-callback (data interface)
  "Callback function for zoom-to-change button - currently just logs"
  (declare (ignore data))
  (%zoom-to interface 0 0)
  (log:info "Zoom to change button pressed for interface: ~a" interface))



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


