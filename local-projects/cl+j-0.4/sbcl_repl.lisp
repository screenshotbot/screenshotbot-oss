;;;
;;;
;;; Copyright (c) 2009, Jean-Claude Beaudoin
;;; All rights reserved by the author.
;;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use, copy,
;;; modify, merge, publish, distribute, sublicense, and/or sell copies
;;; of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.
;;;
;;;
;;;


(defun quit-repl (&key (status 0))
  (declare (type (signed-byte 32) status))
  (throw 'quit-secondary-repl status))

(defun spawn-repl ()
  (let* ((initial-thread sb-thread:*current-thread*)
	 (repl
	  (sb-thread:make-thread
	   #'(lambda ()
	       (let ((sb-int:*repl-prompt-fun*
		      #'(lambda (stream)
			  (fresh-line stream)
			  (write-string "> " stream)))
		     status
		     )
		 (format t "~%You should exit this REPL by calling (~S).~%~%" 'quit-repl)
		 (setq status
		       (catch 'quit-secondary-repl
			 (sb-impl::toplevel-repl nil)))
		 (sb-thread:release-foreground initial-thread) ;
		 status
		 )
	       )
	   :name "Secondary REPL")))
    (sb-thread:release-foreground repl)
    (handler-case 
	(let ((status (sb-thread:join-thread repl)))
	  ;;(format t "Secondary REPL returned with status = ~S~%" status)
	  (sb-ext:quit :unix-status status)
	  )
      (sb-thread:join-thread-error (condition)
	(declare (ignorable condition))
	;;(format t "~%Secondary REPL terminated abnormally!~% condition = ~S~%"
	;;        condition)
	(sb-ext:quit :unix-status -1)
	)
      )
      
    ;;(format t "~%Back to Primary REPL!~%")
    ;;(values)
    )
  )
