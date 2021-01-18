(in-package :bknr.utils)

(define-constant +whitespace-chars+
  '(#\Space #\Newline #\Tab #\Linefeed))

(defun whitespace-char-p (c)
  (member c +whitespace-chars+))

(defun whitespace-p (c-or-s)
  (cond ((stringp c-or-s)
	 (every #'whitespace-char-p c-or-s))
	((characterp c-or-s)
	 (whitespace-char-p c-or-s))
	(t nil)))

(defun bknr-read-string-until (s endchar &key (start nil)
			       (test #'eql) test-not (unread-endchar nil))
  (do ((c (peek-char nil s nil 'eof)
	  (peek-char nil s nil 'eof))
       (ret (copy-list start))
       (escaped nil (and (not escaped) (eql c #\\))))
      ((or (eq c 'eof)
	   (and (not escaped)
		(if test-not
		    (not (funcall test-not c endchar))
		    (funcall test c endchar))))
       (unless (or unread-endchar
		   (eq c 'eof))
	 (push (read-char s) ret))
       (coerce (nreverse ret) 'string))
    (push (read-char s) ret)))

(defun bknr-read-string (s)
  (bknr-read-string-until s #\" :start '(#\")))

(defun bknr-read-comment (s)
  (bknr-read-string-until s '(#\Newline #\Linefeed) :test #'member :unread-endchar t))

(defun bknr-read-whitespace (s)
  (bknr-read-string-until s +whitespace-chars+ :test-not #'member :unread-endchar t))

(defun bknr-read-something (s)
  (bknr-read-string-until s (append +whitespace-chars+ '(#\( #\" #\))) :test #'member
			:unread-endchar t))

(defun bknr-read-delimited-list (s endchar &optional eof-error-p eof-value collect-whitespace)
  (do ((c (peek-char nil s nil 'eof)
	  (peek-char nil s nil 'eof))
       ret)
      ((or (eq c 'eof)
	   (eq c endchar))
       (when (eq c endchar)
	 (read-char s))
       (nreverse ret))
    (push (bknr-read s eof-error-p eof-value collect-whitespace) ret)))

(defun bknr-read (s &optional (eof-error-p t) eof-value (collect-whitespace t))
  (prog ()
   again
   (let ((c (peek-char nil s eof-error-p eof-value)))
     (cond ((and eof-value
		 (eq c eof-value))
	    (return eof-value))
	   ((eq c #\()
	    (read-char s)
	    (return 
	      (if collect-whitespace
		  (collect-whitespace (bknr-read-delimited-list
				       s #\) eof-error-p eof-value collect-whitespace))
		  (bknr-read-delimited-list
		   s #\) eof-error-p eof-value collect-whitespace))))
	   ((eq c #\")
	      (read-char s)
	    (return (bknr-read-string s)))
	   ((eq c #\;)
	    (return (bknr-read-comment s)))
	   ((eq c #\))
	    (read-char s)
	    (return ")"))
	   ((whitespace-char-p c)
	    (let ((whitespace (bknr-read-whitespace s)))
		(if collect-whitespace
		    (return whitespace)
		    (go again))))
	   (t (return (bknr-read-something s)))))))

(defun collect-whitespace (list)
  (do ((l list (cdr l))
       whitespace)
      ((or (null l)
	   (not (whitespace-p (car l))))
       (cond ((and (null whitespace)
		   (null l))
	      nil)
	     (t (cons (apply #'concatenate 'string (nreverse whitespace))
		      (unless (null l)
			(cons (car l) (collect-whitespace (cdr l))))))))
    (when (> (length (car l)) 0)
      (push (car l) whitespace))))

(defun string-beginning-with-p (string beginning)
  (let ((beginlen (length beginning)))
    (and (stringp string)
	 (and (>= (length string) beginlen)
	      (string-equal (subseq string 0 beginlen) beginning)))))

(defun string-delimited-by-p (string char)
  (and (stringp string)
       (let ((len (length string)))
	 (and (> len 2)
	      (eql (char string 0) char)
	      (eql (char string (1- len)) char)))))

