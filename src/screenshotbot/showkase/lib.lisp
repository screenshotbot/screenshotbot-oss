(defpackage :screenshotbot/showkase/lib
  (:use #:cl
        #:iterate
        #:util/java)
  (:export
   #:*context*
   #:*target-context*
   #:*instrumentation*))
(in-package :screenshotbot/showkase/lib)

(named-readtables:in-readtable java-syntax)

(defvar *context*)
(defvar *target-context*)
(defvar *instrumentation*)

(defun list-methods (class)
  (mapcar #_getName (array->list (#_getMethods class))))


(defun make-provider (module)
  (new-instance (lw-ji:find-java-class (format nil "~aCodegen" module))))

(defun get-metadata (module)
  (#_metadata (make-provider module)))

(defclass component ()
  ((name :initarg :name
         :reader name)
   (height :initarg :height
           :reader height)
   (width :initarg :width
          :reader width)
   (compose-component :initarg :compose-component
                      :reader compose-component)))

(defmethod make-component-view ((self component))
  (let ((view (new-instance (lw-ji:find-java-class "androidx.compose.ui.platform.ComposeView")
                            *context*
                            nil
                            0 #| defStyleAttr? what goes here|#))
        (frame (new-instance #,android.widget.FrameLayout
                             *context*)))

    (#_setContent view (compose-component self))
    (#_addView frame view)

    (values frame view)))

(defun get-components (module)
  (iter (for var in-java  (#_getComponentList (get-metadata module)))
        (collect (make-instance 'component
                                :name (#_getComponentName var)
                                :height (#_getHeightDp var)
                                :width (#_getWidthDp var)
                                :compose-component (#_getComponent var)))))



(defvar *f* 0)

(defun runnable-run (user-data)
  (funcall user-data))

(lw-ji:define-lisp-proxy runnable
  ("java.lang.Runnable"
   ("run" runnable-run :with-user-data t)))

(defun make-runnable (fn)
  (lw-ji:jobject-ensure-global
   (lw-ji:make-lisp-proxy 'runnable :user-data fn)))


(defun on-ui-thread (fn)
  (let* ((looper (#_getMainLooper #,android.os.Looper))
         (handler (new-instance #,android.os.Handler looper)))
    (assert looper)
    (#_post handler (make-runnable fn))))


(defun test-stuff ()
  (on-ui-thread (lambda ()
                  (#_i #,android.util.Log "SbInss" "hello world")
                  (incf *f*)))
  (hcl:android-funcall-in-main-thread
   (lambda ()
     (incf *f*))))

#+nil
(mapcar #_getName (array->list (#_getParameterTypes (second (array->list (#_getDeclaredMethods (lw-ji:find-java-class "androidx.lifecycle.ViewTreeLifecycleOwner")))))))

(lw-ji:define-java-callers "androidx.lifecycle.ViewTreeLifecycleOwner"
  (set-lifecycle-owner "set"))

(lw-ji:define-java-callers "androidx.savedstate.ViewTreeSavedStateRegistryOwner"
  (set-saved-state-registry-owner "set"))

(defun init-lifecycle-owner (view)
  (let ((lo (new-instance #,androidx.lifecycle.testing.TestLifecycleOwner)))
    (set-lifecycle-owner view lo)))

(defun make-lifecycle-owner ()
  (new-instance #,io.screenshotbot.ViewOwners$MySavedStateRegistryOwner
                (new-instance #,io.screenshotbot.ViewOwners$MyLifecycleOwner)))

(defun lg (msg)
  (#_i #,android.util.Log "SbInss" msg))


(defun screenshot (component)
  (let* ((lock (bt:make-lock))
         (cv (bt:make-condition-variable))
         (res nil))
    (bt:with-lock-held (lock)
      (hcl:android-funcall-in-main-thread
       (lambda ()
         (lg "this is being called")
         (setf res :bar)
         (bt:with-lock-held (lock)
           (multiple-value-bind (frame view)
               (make-component-view component)
             (let ((lo (make-lifecycle-owner)))
               (set-lifecycle-owner frame lo)
               (set-saved-state-registry-owner frame lo)
               #+nil(set-lifecycle-owner view lo))

             ;;(#_setContentView fragment view)
             ;;(init-lifecycle-owner view)
             (let ((detacher (#_dispatchAttach #,com.facebook.testing.screenshot.WindowAttachment
                                               frame))
                   (view-helper (#_setupView #,com.facebook.testing.screenshot.ViewHelpers
                                             frame)))
               (unwind-protect
                    (progn
                      (lg "before setting dim")
                      (#_setExactWidthDp view-helper
                                         (or
                                          (width component)
                                          200))
                      (#_setExactHeightDp view-helper
                                          (or
                                           (height component)
                                           200))

                      (lg "After setting dims")

                      (setf res :car)
                      (setf res (#_draw (#_layout view-helper)))

                      ;;(Setf res :dar)
                      (bt:condition-notify cv))
                 (#_detach detacher)))))))
      (bt:condition-wait cv lock))
    res))

(defun write-bitmap (bitmap file)
  (log:info "Writing bitmap to ~a" file)
  (#_writeBitmap *instrumentation* bitmap (namestring file)))

(defun get-screenshot-dir ()
  (format nil "~a/" (#_toString (#_getDataDir *target-context*))))

(defun launch-activity ()
  (let ((context *context*))
    (let ((intent (new-instance #,android.content.Intent
                                context (lw-ji:find-java-class "com.example.project.MainActivity"))))
      (#_setFlags intent 268435456 )
      (#_startActivity context intent))))

;; (write-bitmap (screenshot (car (get-components "com.airbnb.android.showkasesample.RootModule"))) (path:catfile (get-screenshot-dir) "hello.png"))
