(in-package :bknr.datastore)

(defun save-cmucl-clean-slime-debugger ()
  "Called in *after-save-initializations* because cores dumped
when slime is running has this bound. TODO"
  (format t "~&clearing debugger hook (~A)" cl:*debugger-hook*)
  (setf cl:*debugger-hook* nil))

(defun save-cmucl-close-fd-handlers ()
  (loop for handler in lisp::*descriptor-handlers*
     when (> (lisp::handler-descriptor handler) 2)
     do (SYSTEM:REMOVE-FD-HANDLER handler)))

(defun save-cmucl-inits (corefilepath)
  "called in the child process"
  (save-cmucl-close-fd-handlers)
  (mp::shutdown-multi-processing)
  (when cl:*debugger-hook*
    (warn "CHILD: setting debugger-hook to NIL")
    (setf cl:*debugger-hook* nil)	; does not work!
    (pushnew 'save-cmucl-clean-slime-debugger ext:*after-save-initializations*))
  (pushnew 'system::reinitialize-global-table ext:*after-save-initializations*)
  (ext:save-lisp corefilepath)
  (warn "CHILD: strangely survived. killing.")
  (unix:unix-exit 1))

(defun snapshot-core (&optional (corefilepath  "/tmp/bknr.core"))
  (cond ((zerop (unix:unix-fork))
	 (save-cmucl-inits corefilepath))
	(t (alien:alien-funcall
	    (alien:extern-alien "wait"
				(alien:function alien:unsigned alien:unsigned))
	    0)))
  (warn "PARENT saved"))
