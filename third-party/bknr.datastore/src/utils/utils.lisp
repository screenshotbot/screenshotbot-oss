(in-package :bknr.utils)

(defmacro define-constant (name value &optional doc)
  "Macro for use in place of defconstant in order to  make SBCL compiler happy"
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
                           ,@(when doc (list doc))))
;;; date format

;; Zeitzone fuer Mail-Zeitstempel
(defparameter *mail-timezone* "+0100")

(defun month-name (month)
  (elt #("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec") (1- month)))

(defun format-date-time (&optional universal-time &key stream
			 (show-year t) (show-month t)
			 (show-date t) (show-time t) (show-weekday nil)
			 (show-seconds t)
			 vms-style us-style mail-style xml-style js-style)
  (or show-date show-time
      (warn "format-date-time: show-date and show-time are nil, nothing printed"))
  (multiple-value-bind (sec min hour day month year weekday)
      (decode-universal-time (or universal-time (get-universal-time)))
    (when (equal show-year :short)
      (setq year (mod year 100)))
    (when show-weekday
      (setf weekday (nth weekday '("MON" "TUE" "WED" "THU" "FRI" "SA" "SO"))))
    (let ((s (if stream stream (make-string-output-stream))))
      (cond
	(mail-style
	 (format s "~A, ~2,'0D ~A ~4D ~2,'0D:~2,'0D:~2,'0D ~A"
		 (elt #("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun") weekday)
		 day (month-name month) year
		 hour min sec *mail-timezone*))
	(us-style
	 (format s "~A ~2,'0D, ~A"
		 (month-name month) day year))
	(vms-style
	 (when show-date
	   (setf month (nth (- month 1) '("JAN" "FEB" "MAR" "APR" "MAY" "JUN" "JUL" "AUG" "SEP" "OCT" "NOV" "DEC")))
	   (format s "~2,' d-~a-~d" day month year))
	 (when (and show-date show-time)
	   (princ #\Space s))
	 (when show-time
	   (format s "~2,' d:~2,'0d:~2,'0d" hour min sec)))
	(xml-style
	 (format s "~4,'0d~2,'0d~2,'0dT~2,'0d~2,'0d~2,'0d"
		 year month day hour min sec))
	(js-style
	 (format s "new Date(~D, ~D, ~D, ~D, ~D, ~D)"
		 year (1- month) day hour min sec))
	(t
	 (when show-weekday
	   (format s "~a " weekday))
	 (when show-date
	   (format s "~2,'0d." day)
	   (when show-month
	     (format s "~2,'0d." month))
	   (when show-year
	     (format s "~4,'0d" year))
	   (when (and show-date show-time)
	     (princ #\Space s)))
	 (when show-time
	   (format s "~2,'0d:~2,'0d" hour min))
	 (when (and show-seconds show-time)
	   (format s ":~2,'0d" sec))))
      (unless stream
	(get-output-stream-string s)))))

(defun format-time-interval (seconds)
  (format nil "~d:~2,'0d" (floor (/ seconds 60)) (mod seconds 60)))

(defun format-duration (duration)
  (cond
    ((> duration (* 24 3600)) (format nil "~ad" (round (/ duration (* 24 3600)))))
    ((> duration 3600)        (format nil "~dh" (round (/ duration 3600))))
    ((> duration 60)          (format nil "~am" (round (/ duration 60))))
    (t	                      (format nil "~as" duration))))

(defun month-interval (month year)
  "Returns two values, the first and last second of the given month"
  (values
   (encode-universal-time 0 0 0 1 month year)
   (- (if (= 12 month)
	  (encode-universal-time 0 0 0 1 1 (+ 1 year))
	  (encode-universal-time 0 0 0 1 (+ 1 month) year))
      1)))

(defun day-interval (day month year)
  "Returns two values, the first and last second of the given day"
  (values
   (encode-universal-time 0 0 0 day month year)
   (encode-universal-time 59 59 23 day month year)))

(defun year-interval (year)
  (values
   (encode-universal-time 0 0 0 1 1 year)
   (encode-universal-time 59 59 23 31 12 year)))

(defun get-hourtime (time)
  (multiple-value-bind (second minute hour)
      (decode-universal-time time)
    (declare (ignore second minute))
    hour))

(defun get-daytime (time)
  (multiple-value-bind (second minute hour date month year day)
      (decode-universal-time time)
    (declare (ignore second minute hour day))
    (nth-value 0 (day-interval date month year))))

(defun get-monthtime (time)
  (multiple-value-bind (second minute hour date month year day)
      (decode-universal-time time)
    (declare (ignore second minute hour date day))
    (nth-value 0 (month-interval month year))))

(defun previous-day (count &key (start (get-universal-time)))
  (- start (* count (* 24 3600))))

(defun next-day (count &key (start (get-universal-time)))
  (+ start (* count (* 24 3600))))

(defun month-num-days (month year)
  (multiple-value-bind (start end) (month-interval month year)
    (nth-value 0 (round (/ (- end start) (* 24 3600))))))

(defun timetag ()
  (multiple-value-bind (second minute hour date month year)
      (decode-universal-time (get-universal-time) 0)
    (format nil
	    "~d~2,'0d~2,'0dT~2,'0d~2,'0d~2,'0d"
	    year month date hour minute second)))

(defun daytag ()
  (multiple-value-bind (second minute hour date month year)
      (decode-universal-time (get-universal-time) 0)
    (declare (ignore second minute hour))
    (format nil
	    "~d~2,'0d~2,'0d"
	    year month date)))

;;; run shell command and return output as string
;;; (lifted out of ASDF)

(defun run-shell-command-to-string (control-string &rest args)
  "Interpolate ARGS into CONTROL-STRING as if by FORMAT, and
synchronously execute the result using a Bourne-compatible shell.
  Returns the output and the shell's exit code."
  (let* ((result)
	 (command (apply #'format nil control-string args))
	 (str (with-output-to-string (s)
		(setf result (run-shell-command command s)))))
    (values str result)))

(defun run-shell-command (command s)
  #+abcl
  (ext:run-shell-command command :output s)

  #+allegro
  ;; will this fail if command has embedded quotes - it seems to work
  (multiple-value-bind (stdout stderr exit-code)
      (excl.osi:command-output
       (format nil "~a -c \"~a\""
	       #+mswindows "sh" #-mswindows "/bin/sh" command)
       :input nil :whole nil
       #+mswindows :show-window #+mswindows :hide)
    (format s "~{~&; ~a~%~}~%" stderr)
    (format s "~{~&; ~a~%~}~%" stdout)
    exit-code)

  #+clisp                     ;XXX not exactly s, I know
  (ext:run-shell-command  command :output :terminal :wait t)

  #+clozure
  (nth-value 1
	     (ccl:external-process-status
	      (ccl:run-program "/bin/sh" (list "-c" command)
			       :input nil :output s
			       :wait t)))

  #+ecl ;; courtesy of Juan Jose Garcia Ripoll
  (si:system command)

  #+gcl
  (lisp:system command)

  #+lispworks
  (system:call-system-showing-output
   command
   :shell-type "/bin/sh"
   :show-cmd nil
   :prefix ""
   :output-stream s)

  #+sbcl
  (sb-ext:process-exit-code
   (apply #'sb-ext:run-program
	  #+win32 "sh" #-win32 "/bin/sh"
	  (list  "-c" command)
	  :input nil :output s
	  #+win32 '(:search t) #-win32 nil))

  #+(or cmu scl)
  (ext:process-exit-code
   (ext:run-program
    "/bin/sh"
    (list  "-c" command)
    :input nil :output s))

  #-(or abcl allegro clisp clozure cmu ecl gcl lispworks sbcl scl)
  (error "RUN-SHELL-COMMAND not implemented for this Lisp")
  )


;;; local hostname

(defun hostname (&key (strip-domain t))
  (let ((hostname
	 #+allegro (sys:getenv "HOST")
	 #+cmu (cdr (assoc :host ext:*environment-list*))
	 #+openmcl (ccl::getenv "HOST")
	 #+sbcl (sb-ext:posix-getenv "HOST")))
    (unless hostname
      (error "HOST environment variable not set, can't continue"))
    (if strip-domain
	(regex-replace "\\..*$" hostname "")
	hostname)))


;;; filesystem functions

(defun directory-empty-p (pathname)
  (zerop (length (directory pathname))))

(defun subdir-p (subdir dir)
  (let ((subdir (probe-file subdir))
	(dir (probe-file dir)))
    (when (and subdir dir)
      (equal (subseq (pathname-directory subdir)
		     0 (length (pathname-directory dir)))
	     (pathname-directory dir)))))

(defun move-file (file1 file2)
  #+(or allegro openmcl)
  (rename-file file1 file2)
  #+cmu
  (unix:unix-rename (namestring file1)
		    (namestring file2))
  #+sbcl
  (sb-unix:unix-rename (namestring file1)
		       (namestring file2)))

(defun make-temporary-pathname (&key (defaults nil) (name "tmp"))
  (let ((args (when defaults `(:defaults ,defaults))))
   (loop for file = (apply 'make-pathname :name (format nil "~A-~A-~A"
						                                name
                                                        (get-universal-time)
                                                        (random most-positive-fixnum))
                            args)
         while (probe-file file)
         finally (return file))))

(defmacro with-temporary-file ((var &rest args) &body body)
  `(let ((,var (make-temporary-pathname ,@args)))
     (unwind-protect
	  (progn ,@body)
       (when (probe-file ,var)
	 (delete-file ,var)))))

(defun parent-directory (pathname)
  (make-pathname :directory (butlast (pathname-directory pathname))
		 :defaults pathname))

;;; list functions

(defun delete-first (obj list &key (test #'eql))
  (if (funcall test (first list) obj)
      (cdr list)
      (do ((l list (cdr l))
	   (last nil l))
	  ((null l) list)
	(when (funcall test (car l) obj)
	  (rplacd last (cdr l))
	  (return list)))))

(defun make-keyword-from-string (string)
  (if (keywordp string)
      string
      (nth-value 0 (intern (string-upcase (regex-replace-all "\\s+" string "-")) 'keyword))))

(defun assoc-values (item alist &key (test #'equal))
  (mapcan #'(lambda (x) (and (funcall test item (car x))
			     (list (cdr x))))
	  alist))

(defun insert-at-index (idx l elt)
  (cond ((= idx 0)
	 (cons elt l))
	((= idx (1- (length l)))
	 (append l (list elt)))
	(t (append (subseq l 0 idx)
		   (list elt)
		   (subseq l idx)))))

(defun find-neighbourhood (elt list depth &key (test #'eql))
  (loop for rest on list
	with seen = list and i = 0
	when (funcall test elt (car rest))
	do (return (subseq seen 0 (+ 1 depth i)))
	do (if (>= i depth) (setf seen (cdr seen)) (incf i))))

(defun assoc-to-keywords (args)
  (loop for (key . value) in args
	nconc (list (make-keyword-from-string key) value)))

(defun group-by (list num)
  (loop for group on list by #'(lambda (seq) (subseq seq num))
	collect (subseq group 0 num)))

(defun group-on (list &key (test #'eql) (key #'identity) (include-key t))
  (let ((hash (make-hash-table :test test))
        keys)
    (dolist (el list)
      (let ((key (funcall key el)))
        (unless (nth-value 1 (gethash key hash))
          (push key keys))
        (push el (gethash key hash))))
    (mapcar (lambda (key) (let ((keys (nreverse (gethash key hash))))
                            (if include-key
                                (cons key keys)
                                keys)))
            (nreverse keys))))

(defun count-multiple (objects &rest keys)
  (let ((hash-tables (loop for i from 1 to (length keys)
			   collect (make-hash-table :test #'equal)))
	(sum 0))
    (dolist (object objects)
      (incf sum)
      (loop for key in keys
	    for i from 0
	    do (incf-hash (funcall key object) (nth i hash-tables))))
    (apply #'values sum hash-tables)))

#+no-alexandria
(defun rotate (list)
  (when list
    (append (cdr list) (list (car list)))))

(defun nrotate (list)
  (when list
    (let ((first (pop list)))
      (rplacd (last list) (list first))
      list)))

(defun genlist (from to)
  (loop for i from from to to
	collect i))

(defun shift-until (list num &key (test #'>=))
  (do* ((l list (cdr l))
	(smaller nil (cons i smaller))
	(i (car l) (car l)))
       ((funcall test i num)
	(append l (nreverse smaller)))))

;;; from norvig
(defun find-all (item sequence &rest keyword-args
                 &key (test #'eql) test-not &allow-other-keys)
  "Find all those elements of sequence that match item,
  according to the keywords.  Doesn't alter sequence."
  (if test-not
      (apply #'remove item sequence
             :test-not (complement test-not) keyword-args)
      (apply #'remove item sequence
             :test (complement test) keyword-args)))

;;; hash table
(defun hash-to-list (hash &key (key #'cdr) (compare #'>) num)
  (let ((results (sort (loop for key being the hash-key of hash using (hash-value val)
			     collect (cons key val))
		       compare :key key)))
    (if num
	(subseq results 0 num)
	results)))

(defun hash-values (hash)
  (loop for value being the hash-values of hash
	collect value))

(defun hash-keys (hash)
  (loop for key being the hash-keys of hash
	collect key))

(defun incf-hash (key hash &optional (delta 1))
  (if (gethash key hash)
      (incf (gethash key hash) delta)
      (setf (gethash key hash) delta)))


;;; randomize

(defun randomize-list (l)
  (let ((len (length l)))
    (flet ((randomize (l)
	     (let ((x (random len))
		   (mov (pop l)))
	       (insert-at-index x l mov))))
      (dotimes (x len)
	(setf l (randomize l)))))
  l)

(defun random-elts (choices num)
  (subseq (randomize-list choices) 0 num))

;;; hashes
(defun hash-to-hex (vector)
  (format nil "~{~2,'0X~}" (coerce vector 'list)))

(defun md5-string (input-string)
  (apply #'concatenate 'string (mapcar #'(lambda (c)
					   (format nil "~2,'0X" c))
				       (coerce (#+(or cmu ccl) md5sum-sequence #+sbcl md5sum-string input-string) 'list))))

#+(or)
(defun md5-string (string)
  (hash-to-hex (digest-string :md5 string)))

;;; content-types

(defvar *image-type<->content-type* '((:jpg . "image/jpeg")
				      (:png . "image/png")
				      (:gif . "image/gif")))

(defun pathname-type-symbol (pathname)
  (intern (string-upcase (pathname-type pathname)) 'keyword))

(defun image-content-type (type-symbol)
  "Return the MIME type of the image - If the type-symbol is a string,
it is assumed that the string specifies the MIME type."
  (if (keywordp type-symbol)
      (cdr (find type-symbol *image-type<->content-type* :test #'equal :key #'car))
      type-symbol))

(defun image-type-symbol (content-type)
  (car (find content-type *image-type<->content-type* :test #'equal :key #'cdr)))

(defun pathname-content-type (pathname)
  (image-content-type (pathname-type-symbol pathname)))

;;; utf08
(defun convert-utf8-to-latin1 (string &key (ignore-errors t))
  (declare (string string) (optimize (speed 3)))
  (with-output-to-string (stream)
    (let ((length (length string))
          (index 0))
      (declare (fixnum length index))
      (loop
       (unless (< index length) (return nil))
	   (let* ((char (char string index))
		  (code (char-code char)))
	     (restart-case
		 (handler-bind
		     ((error #'(lambda (c)
				 (if ignore-errors
				     (invoke-restart 'ignore-byte)
				     (error c)))))
		   (cond
		     ((< code #x80) ; ASCII
		      (write-char char stream)
		      (incf index 1))
		     ((< code #xC0)

		      ;; We are in the middle of a multi-byte sequence!
		      ;; This should never happen, so we raise an error.
		      (error "Encountered illegal multi-byte sequence."))
		     ((< code #xC4)
		      ;; Two byte sequence in Latin-1 range
		      (unless (< (1+ index) length)
			(error "Encountered incomplete two-byte sequence."))
		      (let* ((char2 (char string (1+ index)))
			     (code2 (char-code char2)))
			(unless (and (logbitp 7 code2) (not (logbitp 6 code2)))
			  (error "Second byte in sequence is not a continuation."))
			(let* ((upper-bits (ldb (byte 2 0) code))
			       (lower-bits (ldb (byte 6 0) code2))
			       (new-code (dpb upper-bits (byte 2 6) lower-bits)))
			  (write-char (code-char new-code) stream)))
		      (incf index 2))
		     ((>= code #xFE)
		      ;; Ignore stray byte-order markers
		      (incf index 1))
		     (t
		      (error (format nil "Multi-byte sequence ~d (~d) outside Latin-1 range."
				     code char)))))
	       (ignore-byte ()
		 :report "Ignore byte"
		 (incf index 1))
	       (ignore-n-bytes (n)
		 :report "Ignore some bytes"
		 :interactive (lambda () (format t "Enter a new value: ")
				      (multiple-value-list (eval (read))))
		 (incf index n))
	       (write-another-char (b)
		 :report "Write a new char"
		 :interactive (lambda () (format t "Enter a new char: ")
				      (multiple-value-list (eval (read))))
		 (write-char b stream)
		 (incf index 1))
	       (write-char ()
		 :report "Write byte to latin-1 string"
		 (write-char char stream)
		 (incf index 1))))))))

;;; stirng functions
(defun find-matching-strings (regexp strings &key case-sensitive)
  (let ((scanner (create-scanner regexp :case-insensitive-mode (not case-sensitive))))
    (remove-if-not #'(lambda (str)
		       (scan scanner str)) strings)))

;;; stream functions
;;; from macho (by Miles Egan)
(defun make-extendable-string ()
  "Creates a resizable string."
  (make-array 0 :fill-pointer t :adjustable t :element-type 'base-char))

(defun read-delimited (stream token)
  "Reads stream up to delimiter."
  (let ((string (make-extendable-string)))
    (handler-case
        (loop with tok-length = (length token)
              with state = 0
              initially (vector-push-extend (read-char stream) string) ;; skip first char
              for c = (read-char stream)
              while (< state tok-length)
              do (let ((match (char= c (aref token state))))
                   (cond
                     ((and (> state 0) (not match))
                      (unread-char c stream)
                      (setf state 0))
                     (t
                      (if match (incf state))
                      (vector-push-extend c string))))
              finally (progn
                        (file-position stream (- (file-position stream) tok-length))
                        (adjust-array string (- (length string) tok-length) :fill-pointer t)
                        (return (values string t))))
      (end-of-file () (values (if (> (length string) 0) string nil)
                              nil)))))

(defun read-file (stream)
  "Reads entire contents of stream into a string."
  (loop with result = (make-extendable-string)
        for c = (read-char stream nil)
        while c
        do (vector-push-extend c result)
        finally (return result)))

(defun remove-keys (keys args)
  (loop for (name val) on args by #'cddr
	unless (member name keys)
	append (list name val)))

(defun eval-initargs (initargs)
  (loop for (key value) on initargs by #'cddr
	nconc (list key (eval value))))

#-allegro
(defun file-contents (pathname &key (element-type '(unsigned-byte 8)) (external-format :utf-8))
  (cond
    ((equal element-type '(unsigned-byte 8))
     (with-open-file (s pathname :element-type element-type)
       (let ((result
              (make-array (file-length s) :element-type element-type)))
         (read-sequence result s)
         result)))
    ((equal element-type 'character)
     (alexandria:read-file-into-string pathname :external-format external-format))
    (t
     (error "unsupported element type ~A for file-contents" element-type))))

(defun class-subclasses (class)
  "Return a list of the names of all subclasses of a given class."
  (labels ((collect-subclasses (class)
	     (let ((subclasses
		    #+allegro
		     (aclmop:class-direct-subclasses class)
		     #+cmu
		     (pcl:class-direct-subclasses class)
		     #+openmcl
		     (openmcl-mop:class-direct-subclasses class)
		     #+sbcl
		     (sb-mop:class-direct-subclasses class)))
	       (apply #'append subclasses
		      (mapcar #'collect-subclasses subclasses)))))
    (mapcar #'class-name (remove-duplicates (collect-subclasses (if (symbolp class) (find-class class) class))))))

(defun scale-bytes (byte-count)
  (cond
    ((> byte-count (* 1024 1024 1024 1024))
     (format nil "~3,1F TB" (/ byte-count (* 1024 1024 1024 1024))))
    ((> byte-count (* 1024 1024 1024))
     (format nil "~3,1F GB" (/ byte-count (* 1024 1024 1024))))
    ((> byte-count (* 1024 1024))
     (format nil "~3,1F MB" (/ byte-count (* 1024 1024))))
    ((> byte-count 1024)
     (format nil "~3,1F KB" (/ byte-count 1024)))
    (t
     (format nil "~A" byte-count))))

(defun subseq* (sequence start &optional end)
  "Like SUBSEQ, but limit END to the length of SEQUENCE"
  (subseq sequence start (when end
                           (min end (length sequence)))))
