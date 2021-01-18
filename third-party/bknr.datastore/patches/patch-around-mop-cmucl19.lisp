;;; This patch fixes the problem with get-accessor-method-function
;;; throwing an internal error in cmucl 19
;;;
;;; Not yet in cmucl

(in-package :pcl)

(#+cmu19 ext:without-package-locks
       #-cmu19 progn
  (defun get-accessor-method-function (gf type class slotd)
    (let* ((std-method (standard-svuc-method type))
	   (str-method (structure-svuc-method type))
	   (types1 `((eql ,class) (class-eq ,class) (eql ,slotd)))
	   (types (if (eq type 'writer) `(t ,@types1) types1))
	   (methods (compute-applicable-methods-using-types gf types))
	 ;;; XXX hack??
	   (std-p (or (null (cdr methods))
		      (every #'(lambda (method) (memq (car (method-qualifiers method))
						      '(:around :before :after)))
			     methods)))
	   (old-std-p (null (cdr methods))))
      #+nil
      (unless (eql old-std-p std-p)
	(format t "methods ~s std-p ~A old-std-p ~A~%"
		methods std-p old-std-p)
	(format t "qualifiers ~S~%" (mapcar #'method-qualifiers methods)))
      (values
       (if std-p
	   (get-optimized-std-accessor-method-function class slotd type)
	   (let* ((optimized-std-fn
		   (get-optimized-std-slot-value-using-class-method-function
		    class slotd type))
		  (method-alist
		   `((,(car (or (member std-method methods)
				(member str-method methods)
				(internal-error "In get-accessor-method-function.")))
		      ,optimized-std-fn)))
		  (wrappers
		   ;;
		   ;; This used to be wrapped in 
		   ;; 
		   ;; (unless (and (eq type 'writer)
		   ;;	      (every (lambda (x)
		   ;;		      (eq (car (method-specializers x))
		   ;;			  *the-class-t*))
		   ;;		    methods))
		   ;;
		   ;; but that looks wrong because WRAPPERS nil signals
		   ;; to GET-SECONDARY-DISPATCH-FUNCTION that we are NOT
		   ;; generating code for an emf, which is wrong because
		   ;; we are.
		   ;;
		   ;; See the message from Kevin Rosenberg <kevin@rosenberg.net>
		   ;; to cmucl-imp from Tue, 22 Apr 2003 13:28:23 -0600
		   ;; and the following thread for a test case where this
		   ;; causes problems.
		   ;;
		   ;; gerd, 2003-04-25
		   (let ((wrappers (list (wrapper-of class)
					 (class-wrapper class)
					 (wrapper-of slotd))))
		     (if (eq type 'writer)
			 (cons (class-wrapper *the-class-t*) wrappers)
			 wrappers)))
		  (sdfun (get-secondary-dispatch-function 
			  gf methods types method-alist wrappers)))
	     (get-accessor-from-svuc-method-function class slotd sdfun type)))
       std-p))))