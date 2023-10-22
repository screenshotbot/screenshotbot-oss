(defpackage :screenshotbot/showkase/lib
  (:use #:cl
        #:iterate
        #:util/java)
  (:export
   #:*context*))
(in-package :screenshotbot/showkase/lib)

(named-readtables:in-readtable java-syntax)

(defvar *context*)

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
                            0 #| defStyleAttr? what goes here|#)))
    (#_setContent view (compose-component self))
    view))

(defun get-components (module)
  (iter (for var in-java  (#_getComponentList (get-metadata module)))
        (collect (make-instance 'component
                                :name (#_getComponentName var)
                                :height (#_getHeightDp var)
                                :width (#_getWidthDp var)
                                :compose-component (#_getComponent var)))))

hcl:*android-main-process-for-testing*


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





(defun screenshot (component)
  (let* ((lock (bt:make-lock))
         (cv (bt:make-condition-variable))
         (res nil))
    (bt:with-lock-held (lock)
      (hcl:android-funcall-in-main-thread
       (lambda ()
         (setf res :bar)
         (bt:with-lock-held (lock)
           (let* ((view (make-component-view component))
                  (detacher (#_dispatchAttach #,com.facebook.testing.screenshot.WindowAttachment
                                              view))
                  (view-helper (#_setupView #,com.facebook.testing.screenshot.ViewHelpers
                                            view)))
             (unwind-protect
                  (progn
                    (when (width component)
                      (#_setExactWidthDp view-helper
                                         (width component)))
                    (when (height component)
                      (#_setExactHeightDp view-helper
                                          (height component)))

                    (setf res (#_draw (#_layout view-helper)))
                    (setf res :foo)
                    (bt:condition-notify cv))
               (#_detach detacher))))))
      ;;(bt:condition-wait cv lock)
      res)))

;; (screenshot (car (get-components "com.airbnb.android.showkasesample.RootModule")))
