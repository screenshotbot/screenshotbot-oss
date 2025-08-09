(defpackage :pixel-diff/differ
  (:use #:cl
        #:capi)
  (:import-from #:screenshotbot-js
                #:animate-transform)
  (:import-from #:pixel-diff/about
                #:show-about-dialog)
  (:import-from #:easy-macros
                #:def-easy-macro)
  (:export
   #:create-empty-interface
   #:image-pane))
(in-package :pixel-diff/differ)

(defmacro or-setf (accessor expr)
  `(or
    ,accessor
    (setf ,accessor ,expr)))

(def-easy-macro with-image-access (&binding image-access pane image &key write &fn fn)
  (let ((image-access (gp:make-image-access pane image)))
    (gp:image-access-transfer-from-image image-access)
    (unwind-protect
         (prog1
             (fn image-access)
           (when write
             (gp:image-access-transfer-to-image image-access)))
      (gp:free-image-access image-access))))

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


(defun draw-background (pane x y width height)
  "Draw a solid gray background. In the future we might bring back "
  (gp:draw-rectangle pane
                     0 0
                     (gp:port-width pane)
                     (gp:port-height pane)
                     :filled t
                     :foreground :gray90))



(defun draw-image-callback (pane x y width height)
  "Callback function to draw the image in the display pane"
  (draw-background pane x y width height)
  (maybe-init-core-transform pane (gp:port-width pane) (gp:port-height pane))
  (assert (core-transform pane))
  (assert (image-transform pane))
  (let ((transform (gp:copy-transform (core-transform pane))))
    (gp:postmultiply-transforms
     transform
     (image-transform pane))
    (gp:with-graphics-transform (pane transform)
      (draw-image-layer pane (image1 pane) x y width height)
      (draw-image-layer pane (image2 pane) x y width height)
      (draw-image-layer pane (comparison pane) x y width height))))

(defun maybe-init-core-transform (pane width height)
  (unless (and
           (core-transform pane)
           (eql width (last-width pane))
           (eql height (last-height pane)))
    (let ((image (gp:load-image pane (image (image1 pane)) :editable t)))
      (let ((screenshotbot-js-stubs::*make-matrix-impl* #'gp:make-transform))
        (setf (last-width pane) width)
        (setf (last-height pane) height)
        (setf (core-transform pane)
              (screenshotbot-js::calc-core-transform
               width
               height
               (gp:image-width image)
               (gp:image-height image)))))))

(defclass abstract-image-layer ()
  ((cached-image :initform nil
                 :accessor cached-image)
   (alpha :initarg :alpha
          :accessor alpha)
   (name :initarg :name
         :reader image-layer-name)) )

(defclass image-layer (abstract-image-layer)
  ((image :initarg :image
          :reader image)))

(defmethod read-image (pane (self image-layer))
  (or-setf
   (cached-image self)
   (gp:load-image pane (image self) :editable t)))

(defclass image-pane (output-pane)
  ((image1 :initarg :image1 :initform nil
           :reader image1)
   (image2 :initarg :image2 :initform nil
           :reader image2)
   (comparison :initarg :comparison :initform nil
               :reader comparison)
   (press-start :initform nil
                :accessor press-start
                :documentation "The coordinates of an initial press start")
   (scroll-max :initarg :scroll-max
               :accessor scroll-max)
   (last-width :initform nil
               :accessor last-width)
   (last-height :initform nil
                :accessor last-height)
   (image-transform :initform (gp:make-transform 1 0 0 1 0 0)
                    :accessor image-transform
                    :documentation "The transform for the image")   
   (core-transform :initform nil
                   :accessor core-transform))
  (:default-initargs :draw-with-buffer t
   ;; Is there a more systematic way to figure out this number? It's
   ;; probably going to be proportional to how many pixels move with
   ;; one typical mouse-wheel movement.
                     :scroll-max (* 4 (capi:screen-height (capi:convert-to-screen)))
                     :display-callback 'draw-image-callback
                     :resize-callback 'image-pane-resize-callback
                     :create-callback 'image-pane-create-callback
                     :coordinate-origin :fixed-graphics))

(defmethod initialize-instance :around ((self image-pane) &rest args &key scroll-max &allow-other-keys)
  (apply #'call-next-method
         self
         :scroll-height scroll-max
         :scroll-initial-y (floor scroll-max 2)
         args))

(defun image-pane-create-callback (pane)
  "Callback function called when image pane is created"

  ;; It is safe to disable this for a second, if you're debugging any
  ;; code releated to zooming/scrolling.
  (capi:simple-pane-show-scroll-bars pane :vertical nil :horizontal nil)

  (capi:set-vertical-scroll-parameters pane :slug-position (floor (scroll-max pane) 2)))


(define-interface image-window ()
  ()
  (:panes
   (image-pane image-pane
               :reader image-pane
               :background :white
               :visible-min-width 400
               :visible-min-height 300
               :vertical-scroll t
               :scroll-callback 'image-pane-scroll-callback
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
   (image-main-layout
    column-layout
    '(image-pane bottom-bar status-text))
   (bottom-bar
    row-layout
    '(view-radio-panel nil  zoom-button)))
  (:menus
   (file-menu "File"
              (("Open (Previous)..." :data :open-previous
                                     :callback-type :interface
                                     :enabled-function 'open-menu-available-p
                                     :callback 'open-previous-callback)
               ("Open (Updated)..." :data :open-updated
                                    :callback-type :interface
                                    :enabled-function 'open-menu-available-p
                                    :callback 'open-updated-callback)))
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
                           :accelerator "-")))
   (help-menu "Help"
              (("About" :data :about
                        :callback-type :interface
                        :callback #'show-about-dialog))))
  (:menu-bar file-menu view-menu help-menu)
  (:default-initargs
   :title "Image Display Window"
   :width (floor (capi:screen-width (capi:convert-to-screen)) 2)
   :height (floor (capi:screen-height (capi:convert-to-screen)) 2)))

(defmethod initialize-instance :after ((self image-window) &rest args &key image1 image2 comparison &allow-other-keys)
  (declare (ignore args))
  (when (and image1 image2 comparison (image-pane self))
    (setf (slot-value (image-pane self) 'image1) image1)
    (setf (slot-value (image-pane self) 'image2) image2)
    (setf (slot-value (image-pane self) 'comparison) comparison)))

(defmethod open-menu-available-p (interface)
  t)

(defun get-current-zoom (image-window)
  "Get the current zoom level from the image-transform of the image-window"
  (when (image-transform (image-pane image-window))
    (let ((transform (image-transform (image-pane image-window))))
      (destructuring-bind (a b c d e f) transform
        (declare (ignore b c d e f))
        a))))

(defun scroll-pos-to-expected-zoom (pane scroll-value)
  (let ((scroll-max (scroll-max pane)))
    (let ((t-param (- 1.0 (/ scroll-value scroll-max))))
      (* 0.1 (expt 100 t-param)))))

(defun zoom-to-scroll-pos (pane zoom)
  (let ((scroll-max (scroll-max pane)))
    (* scroll-max
       (- 1.0
          (/ (log (/ zoom 0.1)) (log 100))))))

(defmethod image-pane-scroll-callback (pane (scroll-dimension (eql :vertical))
                                       (scroll-operation (eql :move))
                                       scroll-value
                                       &key interactive)
  "Handle scroll events for zooming in/out on the image pane"
  (when interactive
    (let ((current-zoom (get-current-zoom (capi:element-interface pane)))
          (current-position (capi:get-vertical-scroll-parameters pane :slug-position)))
      (log:info "Current zoom is: ~a" current-zoom)
      (let* ((expected-zoom (scroll-pos-to-expected-zoom pane scroll-value)))
        (log:info "existing slug pos: ~a" (capi:get-vertical-scroll-parameters pane :slug-position))
        (multiple-value-bind (x y) (capi:current-pointer-position :relative-to pane)
          (log:info "Got pos ~a, ~a " x y)
          (process-zoom pane x (- y current-position)
                        (/ expected-zoom current-zoom)))
        (gp:invalidate-rectangle pane)))))

(defmethod image-pane-scroll-callback (pane (scroll-dimension (eql :vertical))
                                            (scroll-operation (eql :step))
                                            delta
                                            &key interactive)
  ;; We've only seen :step in Windows so far
  (capi:set-vertical-scroll-parameters
   pane
   :slug-position
   (+ (capi:get-vertical-scroll-parameters pane :slug-position)
      (* delta (/ (scroll-max pane) 100))))
  (image-pane-scroll-callback
   pane :vertical :move
   (capi:get-vertical-scroll-parameters pane :slug-position) :interactive t))

(defmethod image-pane-scroll-callback (pane direction scroll-operation scroll-value &key interactive &allow-other-keys)
  (when interactive
    (log:info "scrolled (unhandled) ~a ~a ~a ~a" direction scroll-operation scroll-value interactive)))



(defun open-image-file (interface slot-name prompt-title)
  "Generic function for opening image files and updating the interface"
  (let ((image-layer (slot-value (image-pane interface) slot-name))
        (file (capi:prompt-for-file prompt-title
                                    :operation :open
                                    :filter "*.png;*.jpg;*.jpeg;*.bmp;*.gif"
                                    :filters '("Image files" "*.png;*.jpg;*.jpeg;*.bmp;*.gif"
                                              "All files" "*.*"))))
    (when file
      (let ((new-image-layer (make-instance 'image-layer
                                            :image (namestring file)
                                            :alpha (alpha image-layer))))
        (setf (slot-value (image-pane interface) slot-name) new-image-layer)
        (setf (core-transform (image-pane interface)) nil)
        (setf (slot-value (image-pane interface) 'comparison)
              (make-instance 'comparison-image-layer
                             :image1-layer (image1 (image-pane interface))
                             :image2-layer (image2 (image-pane interface))
                             :alpha 1))
        (gp:invalidate-rectangle (image-pane interface))))))

(defun open-previous-callback (interface)
  "Callback function for opening a previous image file"
  (open-image-file interface 'image1 "Select Previous Image"))

(defun open-updated-callback (interface)
  "Callback function for opening an updated image file"
  (open-image-file interface 'image2 "Select Updated Image"))



(defun view-radio-panel-callback (item interface)
  "Callback function for view radio panel selection changes"
  (log:debug "View changed to: ~a" item)
  (let ((pane (image-pane interface)))
    (case item
     (:previous
      (setf (alpha (image1 pane)) 1.0)
      (setf (alpha (image2 pane)) 0.0)
      (setf (alpha (comparison pane)) 0.0))
     (:diff
      (setf (alpha (image1 pane)) 0.1)
      (setf (alpha (image2 pane)) 0.0)
      (setf (alpha (comparison pane)) 1.0))
     (:updated
      (setf (alpha (image1 pane)) 0.0)
      (setf (alpha (image2 pane)) 1.0)
      (setf (alpha (comparison pane)) 0.0))))
  (gp:invalidate-rectangle (image-pane interface)))


(defun render-color (color)
  (format nil "#~2,'0x~2,'0x~2,'0x~2,'0x"
          (floor (* 255 (color:color-red color)))
          (floor (* 255 (color:color-green color)))
          (floor (* 255 (color:color-blue color)))
          (floor (* 255 (color:color-alpha color)))))


(defun get-image-layer-color (pane image-layer image-x image-y)
  "Get the color of a pixel from an image layer at the specified coordinates"
  (when image-layer
    (let ((image (read-image pane image-layer)))
      (when (and image 
                 (< image-x (gp:image-width image))
                 (< image-y (gp:image-height image)))
        (with-image-access (image-access pane image)
         (color:unconvert-color
          pane
          (gp:image-access-pixel image-access image-x image-y)))))))

(defun image-pane-mouse-move (pane x y)
  "Handle mouse movement over image pane to show pixel information"
  (let ((y (- y (capi:get-vertical-scroll-parameters pane :slug-position))))
   (let ((interface (capi:element-interface pane)))
     (when (and (core-transform pane) (image-transform pane))
       (let ((combined-transform (gp:copy-transform (core-transform pane))))
         (gp:postmultiply-transforms combined-transform (image-transform pane))
         (setf
          (capi:display-pane-text (status-text interface))
          (or
           (multiple-value-bind (image-x image-y)
               (gp:transform-point (gp:invert-transform combined-transform) x y)
             (let ((image-x (round image-x))
                   (image-y (round image-y)))
               (when (and (>= image-x 0) (>= image-y 0))
                 (let* ((color-before (get-image-layer-color pane (image1 pane) image-x image-y))
                        (color-after (get-image-layer-color pane (image2 pane) image-x image-y)))
                   (cond
                     ((and color-before color-after
                           (color:colors= color-before color-after))
                      (format nil "Identical color: ~a" (render-color color-before)))
                     ((and color-before color-after)
                      (format nil "Color changed from ~a to ~a" (render-color color-before) (render-color color-after)))
                     (color-before
                      (format nil "Color was ~a, now out of bounds" (render-color color-before)))
                     (color-after
                      (format nil "Out of bounds earlier, not color is: ~a" (render-color color-after) ))
                     (t
                      (log:warn "Not showing colors at (~a,~a): ~a, ~a" image-x image-y color-before color-after)
                      nil))))))
           "Ready - Move mouse over image to see pixel info")))))))



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
    (let* ((image (gp:load-image pane (image (image1 (image-pane interface))) :editable t))
           (start-mat (transform-to-3dmat (image-transform pane)))
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
         (setf (image-transform pane)
               (3dmat-to-transform
                (animate-transform start-mat final-mat progress)))
         (invalidate-scroll-position interface)
         (gp:invalidate-rectangle pane))
       :finally finally))))

(defmethod invalidate-scroll-position (interface)
  (let ((pane (image-pane interface)))
    (capi:set-vertical-scroll-parameters pane :slug-position (zoom-to-scroll-pos
                                                              pane
                                                              (get-current-zoom interface)))))


(defmethod find-non-transparent-pixel (pane (image gp:image))
  (with-image-access (image-access pane image)
   (let ((width (gp:image-width image))
         (height (gp:image-height image)))
     (loop for y from 0 below height do
       (loop for x from 0 below width do
         (let ((color (color:unconvert-color
                       pane
                       (gp:image-access-pixel image-access x y))))
           (when (> (color:color-alpha color) 0.5)
             (hcl:gc-generation t)
             (return-from find-non-transparent-pixel (values x y))))))
     nil)))

(defun zoom-to-change-callback (data interface)
  "Callback function for zoom-to-change button - currently just logs"
  (declare (ignore data))
  (multiple-value-bind (x y)
      (find-non-transparent-pixel
       (image-pane interface)
       (read-image (image-pane interface)
                   (comparison (image-pane interface))))
    (setf (capi:button-enabled (zoom-button interface))  nil)
    (%zoom-to interface x y
              :finally
              (lambda ()
                (setf (capi:button-enabled (zoom-button interface)) t))))
  (log:debug "Zoom to change button pressed for interface: ~a" interface))

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
                                                            (image-transform pane))
                                                           delta)))
        (gp:postmultiply-transforms
         (image-transform pane)
         dm)
        (invalidate-scroll-position interface)
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
  (log:debug "Got ~a for ~a,~a" character x y)
  (case character
    (#\+
     (process-zoom pane x y 1.3))
    (#\-
     (process-zoom pane x y (/ 1 1.3)))))

(defun image-pane-press (pane x y)
  "Handle mouse button press on image pane"
  (setf (press-start pane)
        (cons x y))
  (log:debug "Image pane press at (~a, ~a)" x y))

(defun image-pane-release (pane x y)
  "Handle mouse button release on image pane"
  (setf (press-start pane) nil)
  (log:debug "Image pane release at (~a, ~a)" x y))

(defun image-pane-drag (pane x y)
  "Handle mouse drag on image pane"
  (log:debug "Image pane drag at (~a, ~a)" x y)
  (when (press-start pane)
    (destructuring-bind (startx . starty)
        (press-start pane)
      (gp:postmultiply-transforms
       (image-transform pane)
       (gp:make-transform 1 0 0 1 (- x startx) (- y starty)))
      (setf (press-start pane)
            (cons x y))
      (gp:invalidate-rectangle pane))))


(defclass comparison-image-layer (abstract-image-layer)
  ((image1-layer :initarg :image1-layer
                 :reader image1-layer)
   (image2-layer :initarg :image2-layer
                 :reader image2-layer)))



(defmethod compare-images (pane (before gp:image) (after gp:image))
  (let ((transparent (color:convert-color pane :transparent))
        (red (color:convert-color pane :red)))
   (let* ((width (max (gp:image-width before) (gp:image-width after)))
          (height (max (gp:image-height before) (gp:image-height after)))
          (result (gp:make-image pane width height :alpha t)))
     (with-image-access (before-access pane before)
       (with-image-access (after-access pane after)
         (with-image-access (result-access pane result :write t)
           (flet ((safe-image-access-pixel (access x y width height)
                    (if (and (< x width) (< y height))
                        (gp:image-access-pixel access x y)
                        (color:convert-color pane :transparent))))
             (loop for y from 0 below height do
               (loop for x from 0 below width do
                 (let ((before-color (color:unconvert-color pane (safe-image-access-pixel before-access x y (gp:image-width before) (gp:image-height before))))
                       (after-color (color:unconvert-color pane (safe-image-access-pixel after-access x y (gp:image-width after) (gp:image-height after)))))
                   (if (color:colors= before-color after-color)
                       (setf (gp:image-access-pixel result-access x y) transparent)
                       (setf (gp:image-access-pixel result-access x y) red)))))))))
     result)))

(defmethod read-image (pane (self comparison-image-layer))
  (or-setf
   (cached-image self)
   (let* ((before-image (read-image pane (image1-layer self)))
          (after-image (read-image pane (image2-layer self)))
          (comparison-image (compare-images pane before-image after-image)))
     comparison-image)))

(defun create-empty-interface (&key image1 image2)
  (let ((image1-layer (make-instance 'image-layer
                                     :image image1
                                     :alpha 0.1))
        (image2-layer (make-instance 'image-layer
                                     :image image2
                                     :alpha 0)))
    (make-instance 'image-window
                   :title "Pixel Diff"
                   :image1 image1-layer
                   :image2 image2-layer
                   :comparison (make-instance 'comparison-image-layer
                                              :image1-layer image1-layer
                                              :image2-layer image2-layer
                                              :alpha 1))))


(defun image-pane-resize-callback (pane x y width height)
  "Handle resize events for the image pane"
  (declare (ignore x y width height))
  (gp:invalidate-rectangle pane))


(defun open-interface (image))


(defun test-example ()
  (display (create-empty-interface
            :image1 "/home/arnold/builds/fast-example/screenshots/image.png"
            :image2 "/home/arnold/builds/fast-example/screenshots-copy/image.png")))


;; (test-example)


