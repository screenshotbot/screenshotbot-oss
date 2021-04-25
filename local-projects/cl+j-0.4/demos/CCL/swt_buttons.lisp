;;;

(in-java-context "hello-swt")

(java-import "org.eclipse.swt.*")
(java-import "org.eclipse.swt.widgets.*")
(java-import "org.eclipse.swt.layout.*")


(defun button1-pushed (event data)
  (declare (ignore event data))
  (format t "~&Button 1 was pushed.~%")
  )

(defun button2-pushed (event data)
  (declare (ignore event data))
  (format t "~&Button 2 was pushed.~%")
  )

(defun button3-pushed (event data)
  (declare (ignore event data))
  (format t "~&Button 3 was pushed.~%")
  )


(defun build-button-set ()
  (with-java-context "hello-swt"
    (let* ((shell (jnew "Shell" (swt:display)))
	   (fillLayout (jnew "FillLayout" #?SWT.VERTICAL))
	   (button1 (jnew "Button" shell #?SWT.PUSH))
	   (button2 (jnew "Button" shell #?SWT.PUSH))
	   (button3 (jnew "Button" shell #?SWT.PUSH))
	  )

      (#]setText shell (jstr "CL+J Demos: SWT button set"))

      (#]setLayout shell fillLayout)
      (#]setText button1 (jstr "button1"))
      (#]addListener button1
		     #?SWT.Selection (swt:make-listener #'button1-pushed))
      (#]setText button2 (jstr "button number 2"))
      (#]addListener button2
		     #?SWT.Selection (swt:make-listener #'button2-pushed))
      (#]setText button3 (jstr "3"))
      (#]addListener button3
		     #?SWT.Selection (swt:make-listener #'button3-pushed))
      (#]pack shell)
      (#]open shell)
      )
    )
  )


(swt:start-swt-event-loop)

(swt::wait-on-swt-event-loop-startup 10)

(swt:In-UI-thread-do #'build-button-set)

