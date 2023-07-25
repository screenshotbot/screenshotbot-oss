(in-package :bknr.datastore)

(cl-interpol:enable-interpol-syntax)

(defvar *store-debug* nil
  "Trace and time execution of transactions")

;;; conditions

(define-condition store-error (error)
  ())

(define-condition not-in-transaction (store-error)
  ()
  (:documentation
   "Signaled when an operation on persistent slots is executed outside
   a transaction context"))

(define-condition store-not-open (store-error)
  ()
  (:documentation
   "Signaled when a transaction is executed on a store that is not
   opened"))

(define-condition store-already-open (store-error)
  ()
  (:documentation
   "Signaled when an attempt is made to open a store with another
   store being open"))

(define-condition invalid-store-random-state (store-error)
  ()
  (:documentation
   "Signaled when the on-disk store random state cannot be read,
   typically because it has been written with another Lisp"))

(define-condition unsupported-lambda-list-option (store-error)
  ((option :initarg :option :reader option))
  (:documentation
   "Signaled when DEFTRANSACTION is used with an unsupported option in
   its lambda list"))

(define-condition default-arguments-unsupported (store-error)
  ((tx-name :initarg :tx-name :reader tx-name)
   (argument :initarg :argument :reader argument))
  (:report (lambda (c stream)
             (format stream "argument ~A defaulted in DEFTRANSACTION ~S"
                     (argument c) (tx-name c))))
  (:documentation
   "Signaled when an argument in a DEFTRANSACTION definition has a
   default declaration"))

(define-condition undefined-transaction (store-error)
  ((tx-name :initarg :tx-name :reader tx-name))
  (:report (lambda (c stream)
             (format stream "undefined transaction ~A in transaction log, please ensure that all the necessary code is loaded."
                     (tx-name c))))
  (:documentation
   "Signaled when a named transaction is loaded from the transaction
   log and no matching function definition could be found"))

(define-condition invalid-transaction-nesting (store-error)
  ()
  (:documentation
   "Signaled when WITH-TRANSACTION forms are nested."))

(define-condition anonymous-transaction-in-transaction (store-error)
  ()
  (:documentation
   "Signaled when an anonymous transaction is started from within another transaction, transactions do not nest."))

(define-condition no-subsystems (store-error)
  ()
  (:documentation
   "Signaled when an attempt is made to snapshot a store without subsystems"))

(define-condition invalid-environment-access (store-error)
  ((function :initarg function))
  (:report (lambda (e stream)
             (with-slots (function) e
               (format stream "A transaction function attempted to access the function ~A which ~
                               would make execution of the transaction non-repeatable."
                       function)))))

;;; Verbose progress reporting of store operations

(defvar *store-verbose* t)

(defun report-progress (fmt &rest args)
  (when *store-verbose*
    (apply #'format *trace-output* fmt args)))

;;; store

(defvar *store*)

(defmacro with-store ((store &key) &body body)
  `(let ((*store* ,store))
     ,@body))

(defclass store ()
  ((directory :initarg :directory
              :accessor store-directory)
   (state :accessor store-state
          :initform :closed
          :documentation "State of the datastore, can be either :closed, :opened or :read-only")
   (transaction-log-stream :accessor store-transaction-log-stream :initform nil)
   (random-state :accessor store-random-state
                 :initform nil)
   (guard :reader store-guard
          :initarg :guard)
   (log-guard :reader store-log-guard
              :initarg :log-guard)
   (subsystems :reader store-subsystems
               :initarg :subsystems)
   (transaction-run-time :accessor store-transaction-run-time
                         :initform 0
                         :documentation "The total run time of all application transaction code since last snapshot"))
  (:default-initargs
   :guard #'funcall
    :log-guard #'funcall
    :subsystems (list (make-instance 'store-object-subsystem))))

(defclass mp-store (store)
  ()
  (:default-initargs :guard (let ((lock (mp-make-lock)))
                              (lambda (thunk)
                                (mp-with-recursive-lock-held (lock)
                                  (funcall thunk))))
    :log-guard (let ((lock (mp-make-lock)))
                 (lambda (thunk)
                   (mp-with-recursive-lock-held (lock)
                     (funcall thunk)))))
  (:documentation
   "Store in which every transaction and operation is protected by a giant lock."))

(defmethod print-object ((store store) stream)
  (print-unreadable-object (store stream :type t)
    (format stream "DIR: \"~a\"" (namestring (store-directory store)))))

(defgeneric initialize-subsystem (subsystem store store-existed-p))
(defmethod initialize-subsystem ((subsystem t) store store-existed-p)
  (declare (ignore store store-existed-p)))

(defmethod initialize-instance :before ((store store) &key (make-default t) directory)
  (assert (and (not (null directory))
               (not (pathname-type directory))
               (not (pathname-name directory)))
          () (format nil "the store :directory argument ~S that was supplied is invalid.  A directory pathname must be specified." directory))
  (when make-default
    (restart-case
        (when (and (boundp '*store*)
                   *store*)
          (error 'store-already-open))
      (close-store ()
        :report "Close the opened store."
        (close-store)))))

(defmethod initialize-instance :after ((store store) &key (make-default t))
  (when (stringp (store-directory store))
    (setf (store-directory store) (pathname (store-directory store))))
  (when make-default
    (setf *store* store))
  (with-store (store)
    (let ((store-existed-p (probe-file (store-current-directory store))))
      (ensure-store-current-directory store)
      (dolist (subsystem (store-subsystems store))
        (when *store-debug*
          (report-progress "Initializing subsystem ~A of ~A~%" subsystem store))
        (initialize-subsystem subsystem store store-existed-p))
      (restore-store store))
    (setf (store-state store) :opened)))

(defmethod close-store-object ((store store))
  (close-transaction-log-stream store)
  (dolist (subsystem (store-subsystems store))
    (close-subsystem store subsystem))
  (setf (store-state store) :closed))

(defun open-store (directory &key (class-name #-mp 'store #+mp 'mp-store) (subsystems (list (make-instance 'store-object-subsystem))))
  (close-store)
  (make-instance class-name :directory directory :subsystems subsystems))

(defun close-store ()
  (makunbound '*store*))

(defmacro with-store-guard ((&optional (store '*store*)) &rest body)
  "Execute BODY in the context of the guard of STORE."
  `(funcall (store-guard ,store) #'(lambda () ,@body)))

(defmacro with-log-guard ((&optional (store '*store*)) &rest body)
  "Execute BODY in the context of the log file guard of STORE."
  `(funcall (store-log-guard ,store) #'(lambda () ,@body)))

(defmacro with-store-state ((state &optional (store '*store*)) &rest body)
  (let ((old-state (gensym)))
    `(let ((,old-state (store-state ,store)))
       (setf (store-state ,store) ,state)
       (unwind-protect (progn ,@body)
         (setf (store-state ,store) ,old-state)))))

;; datastore pathnames

(defgeneric store-current-directory (store)
  (:documentation "Returns the name of the current datastore directory."))

(defmethod store-current-directory ((store store))
  (merge-pathnames (make-pathname :directory '(:relative "current"))
                   (store-directory store)))

(defmethod ensure-store-current-directory ((store store))
  (ensure-directories-exist (store-current-directory store)))

(defmethod store-random-state-pathname ((store store))
  (merge-pathnames #P"random-state" (store-current-directory store)))

(defun initialize-store-random-state (store)
  (with-open-file (f (store-random-state-pathname store)
                     :direction :output :if-does-not-exist :create :if-exists :supersede)
    (report-progress "initializing store random state~%")
    (with-standard-io-syntax
      (prin1 (setf (store-random-state store) (make-random-state t)) f))))

(defmethod ensure-store-random-state ((store store))
  (if (probe-file (store-random-state-pathname store))
      (with-open-file (f (store-random-state-pathname store))
        (restart-case
            (setf (store-random-state store)
                  (handler-case
                      (read f)
                    (error (e)
                      (declare (ignore e))
                      (error 'invalid-store-random-state))))
          (initialize-store-random-state ()
            :report "Initialize the random state of the store.  Use
this to reinitialize the random state of the store when porting over a
store from another compiler. When transactions of the application
depend on the random state, you must snapshot your store before
porting to the new compiler."
            (initialize-store-random-state store))
          (ignore-store-random-state ()
            :report "Ignore the on-disk random state of the store.
Use this if you want to test a store with another compiler, but do not
want to change the store permanently."
            (setf (store-random-state store) (make-random-state t)))))
      (initialize-store-random-state store)))

(defmethod update-store-random-state ((store store))
  (with-open-file (f (store-random-state-pathname store)
                     :direction :output :if-does-not-exist :create :if-exists :supersede)
    (with-standard-io-syntax
      (prin1 (store-random-state store) f))))

(defgeneric store-transaction-log-pathname (store-or-directory)
  (:documentation "Return the pathname of the current transaction log of STORE"))

(defmethod store-transaction-log-pathname ((directory pathname))
  (merge-pathnames "transaction-log" directory))

(defmethod store-transaction-log-pathname ((store store))
  (store-transaction-log-pathname (store-current-directory store)))

(defgeneric store-subsystem-snapshot-pathname (store-or-directory subsystem)
  (:documentation "Return the pathname of the snapshot of SUBSYSTEM of STORE"))

(defmethod store-subsystem-snapshot-pathname ((directory pathname) subsystem)
  (let ((name (string-downcase (symbol-name (class-name (class-of subsystem))))))
    (merge-pathnames (format nil "~a-snapshot" name) directory)))

(defmethod store-subsystem-snapshot-pathname ((store store) subsystem)
  (store-subsystem-snapshot-pathname (store-current-directory store) subsystem))

(defgeneric close-transaction-log-stream (store))
(defgeneric store-transaction-log-stream (store))

(defmethod store-transaction-log-stream :before ((store store))
  (with-slots (transaction-log-stream) store
    (unless transaction-log-stream
      (setf transaction-log-stream (open (store-transaction-log-pathname store)
                                         :element-type '(unsigned-byte 8)
                                         :direction :output
                                         :if-does-not-exist :create
                                         :if-exists :append
                                         #+openmcl :sharing #+openmcl :lock)))))

(defmethod close-transaction-log-stream ((store store))
  (with-slots (transaction-log-stream) store
    (when transaction-log-stream
      (close transaction-log-stream)
      (setf transaction-log-stream nil))))

;;; transaction

;;; named transactions - These transactions carry a name and are logged
;;; to the transaction log file as soon as the transaction function
;;; returns.  They are usually defined using 'deftransaction'.

(defclass transaction ()
  ((function-symbol :initarg :function-symbol
                    :reader transaction-function-symbol
                    :documentation
                    "Symbol of the function called when executing the transaction")
   (args :initarg :args
         :reader transaction-args
         :initform nil)
   (timestamp :initarg :timestamp
              :accessor transaction-timestamp
              :initform (get-universal-time))))

(defvar *current-transaction* nil)

(defun in-transaction-p ()
  (or *current-transaction*
      (eq :restore (store-state *store*))))

(defun current-transaction-timestamp ()
  (transaction-timestamp *current-transaction*))

(defun store-open-p ()
  (not (eq :closed (store-state *store*))))

(defun store-current-transaction ()
  (if (in-transaction-p)
      *current-transaction*
      (error 'not-in-transaction)))

;;; All transactions are executed by an 'executor', which is the store
;;; itself or, in the case of a nested transaction, the parent
;;; transaction.  Named transactions do not explicitly log the nested
;;; transactions as the nesting is implicit, meaning that any repeated
;;; execution of the transactions while rolling forward the
;;; transaction log will automatically repeat the sequence of nested
;;; transaction executions by the program code executed.  Contrasted
;;; to that, an anonymous transaction has no implicit nesting, so any
;;; nested transactions which are called are explicitly logged.

(defgeneric execute-transaction (executor transaction)
  (:documentation "Execute TRANSACTION on EXECUTOR (which may be a store or a transaction scope).")

  (:method :before ((executor t) (transaction t))
           (unless (store-open-p)
             (error 'store-not-open)))

  (:method ((executor transaction) transaction)
    (execute-unlogged transaction)))

(defun find-doc (body)
  "Given a function definition BODY, extract the docstring, if any.
Skips over any declarations that precede the docstring.  See also CLHS
3.4.11"
  (do ((body body (cdr body)))
      ((or (not (listp (car body)))
           (not (eq 'declare (caar body))))
       (when (and (stringp (car body))
                  (cdr body))
         (car body)))))

(defun insert-after-declarations (body forms-to-insert)
  "Given a function definition body, insert FORMS-TO-INSERT after all
declarations and documentation in BODY."
  (loop for rest on body
     for form = (car rest)
     with decls
     with doc
     while (or (and (listp form) (eq 'declare (car form)))
               (and (not doc) (cdr rest) (stringp form)))
     when (stringp form)
     do (setf doc form)
     do (push form decls)
     finally (return-from insert-after-declarations (append (nreverse decls) forms-to-insert rest))))

(defun make-args (args)
  "Parse the lambda list ARGS, returning a list that contains the
arguments in the lambda list prepared so that the list can be applied
to a function accepting that lambda list.

For example:

 (MAKE-ARGS '(A B &OPTIONAL C &REST D &KEY E F)) => (A B C :E E :F F)

It is used to forward arguments to a transaction wrapper generated by
DEFTRANSACTION to the actual transaction so that the wrapper function
can be declared with the lambda list of the transaction function
itself,"
  (do ((args args (cdr args))
       result
       in-keywords-p)
      ((not args)
       (nreverse result))
    (let ((arg (funcall (if (listp (car args)) #'caar #'car) args)))
      (cond
        ((eql #\& (aref (symbol-name arg) 0))
         (case arg
           (&optional)
           (&rest (setf args (cdr args))) ; skip argument, too
           (&key (setf in-keywords-p t))
           (otherwise (error 'unsupported-lambda-list-option :option arg))))
        (t
         (when in-keywords-p
           (push (intern (symbol-name arg) :keyword) result))
         (push arg result))))))

(defmacro deftransaction (name (&rest args) &body body)
  "Define a transaction function tx-NAME and a function NAME executing
tx-NAME in the context of the current store. The arguments to NAME
will be serialized to the transaction-log, and must be supported by
the binary encoder. tx-NAME will be called during a roll-forward to
repeat any effects that the transaction function had on the persistent
store."
  (let ((name name)
        (args args)
        (body body))
    (dolist (arg args)
      (when (listp arg)
        (error 'default-arguments-unsupported :tx-name name :argument (car arg))))
    (let ((tx-name (intern (format nil "TX-~A" name)
                           (symbol-package name))))
      `(progn
         (defun ,tx-name ,args
           ,@(insert-after-declarations body
                                        '((unless (in-transaction-p)
                                            (error 'not-in-transaction)))))
         (defun ,name ,args
           ,@(let ((doc (find-doc body)))
                  (when doc (list (format nil "[Transaction function wrapper ~A invokes a store transaction]~%~A" name doc))))
           ,@(let ((rest (member '&rest args)))
                  (when rest `((declare (ignore ,(second rest))))))
           (execute (make-instance 'transaction
                                   :function-symbol ',tx-name
                                   :timestamp (get-universal-time)
                                   :args (list ,@(make-args args)))))))))

(defmethod encode-object ((object transaction) stream)
  (%write-tag #\T stream)
  (%encode-symbol (transaction-function-symbol object) stream)
  (%encode-integer (transaction-timestamp object) stream)
  (%encode-list (transaction-args object) stream))

(defmethod decode-object ((tag (eql #\T)) stream)
  (make-instance 'transaction
                 :function-symbol (%decode-symbol stream)
                 :timestamp (%decode-integer stream)
                 :args (%decode-list stream)))

(defmethod print-object ((transaction transaction) stream)
  (print-unreadable-object (transaction stream :type t)
    (format stream "~A ~A ~{~A~^ ~}"
            (format-date-time (transaction-timestamp transaction))
            (transaction-function-symbol transaction)
            (transaction-args transaction))))

;;; operations on transactions

(defgeneric execute-unlogged (transaction)
  (:documentation "Invokes the transaction application code
by calling the function named by the transactions' function-symbol.
Called at transaction execution and restore time within different
environment.  May be overridden by specialized transaction types,
e.g. the anonymous transaction which writes a set of transactions
to the log file in an atomic group"))

(defmethod execute-unlogged :around ((transaction transaction))
  "Execute transaction unsafely, catching errors"
  (let (retval
        (execution-time 0))
    (tagbody
     again
       (restart-case
           (let ((start-time (common-lisp::get-internal-run-time))
                 (*random-state* (store-random-state *store*)))
             (setf retval (call-next-method))
             (setf execution-time (- (common-lisp::get-internal-run-time) start-time)))
         (retry-transaction ()
           :report (lambda (stream) (format stream "Retry the transaction ~A." transaction))
           (go again))
	 (skip-transaction ()
	   :report (lambda (stream) (format stream "Skip the transaction ~A." transaction)))))
    (incf (store-transaction-run-time *store*) execution-time)
    retval))

(defmethod execute-unlogged :before ((transaction transaction))
  (when *store-debug*
    (report-progress "executing transaction ~A at timestamp ~A~%" transaction
                     (transaction-timestamp transaction))))

(defmethod execute-unlogged ((transaction transaction))
  (with-store-guard ()
    (let ((*current-transaction* transaction))
      (apply (or (symbol-function (transaction-function-symbol transaction))
                 (error 'undefined-transaction
                        :tx-name (transaction-function-symbol transaction)))
             (transaction-args transaction)))))

(defun fsync (stream)
  ;; FINISH-OUTPUT macht leider auch nichts anderes als FORCE-OUTPUT,
  ;; dabei waere sync()-Semantik zu erwarten.
  (finish-output stream)
  #+cmu
  (unix:unix-fsync (kernel::fd-stream-fd stream))
  #+sbcl
  (sb-posix:fsync (sb-sys:fd-stream-fd stream)))

(defvar *disable-sync* nil)

(defmacro without-sync (() &body body)
  ;; Bei laengeren Importvorgaengen benoetigt das syncen des Transaktionslogs
  ;; viel Zeit, ist aber an der Stelle nicht notwendig.
  ` (unwind-protect
         (let ((*disable-sync* t))
           ,@body)
      (with-log-guard ()
        (fsync (store-transaction-log-stream *store*)))))

(defmacro with-transaction-log ((transaction) &body body)
  (check-type transaction symbol) ; otherwise care for multiple evaluation
  `(with-store-guard ()
     (when (in-transaction-p)
       (error 'invalid-transaction-nesting))
     (with-store-state (:transaction)
       (prog1
           (let ((*current-transaction* ,transaction))
             ,@body)
         (with-log-guard ()
           (let ((out (store-transaction-log-stream *store*)))
             (encode ,transaction out)
             (unless *disable-sync*
               (fsync out))))))))

(defvar *transaction-statistics* (make-statistics-table))

(defmethod execute-transaction ((store store) (transaction transaction))
  (with-statistics-log (*transaction-statistics* (transaction-function-symbol transaction))
    (with-transaction-log (transaction)
      (execute-unlogged transaction))))

(defun execute (transaction)
  "Interface routine to execute a transaction, called through
the deftransaction macro and by subsystems.  Executes the
transaction either with the store or with the currently active
transaction, if any."
  (execute-transaction (if (in-transaction-p)
                           *current-transaction*
                           *store*)
                       transaction))

;;; anonymous transactions - During execution of such a transactions,
;;; nothing is written to a log.  After leaving the body of the
;;; with-transaction block, all transactions which have been executed
;;; are written to the log as a group which will be restored atomically.

;;; The actual writing to the transaction log is performed by the
;;; with-transaction macro.

;;; An anonymous transaction has an optional label which is stored in
;;; the transaction log in order to make the source code location where
;;; the actual transaction code lives identifieable.

(defclass anonymous-transaction (transaction)
  ((label :initarg :label
          :accessor anonymous-transaction-label
          :initform (error "missing label in anonymous transaction definition"))
   (log-buffer :initarg :log-buffer
               :accessor anonymous-transaction-log-buffer
               :initform (flex:make-in-memory-output-stream))
   (undo-log :initform nil
             :accessor anonymous-transaction-undo-log)))

(defmethod print-object ((transaction anonymous-transaction) stream)
  (print-unreadable-object (transaction stream :type t)
    (format stream "~A ~A (~A)"
            (format-date-time (transaction-timestamp transaction))
            (anonymous-transaction-label transaction)
            (class-name (class-of (anonymous-transaction-log-buffer transaction))))))

(defmethod in-anonymous-transaction-p ()
  (subtypep (type-of *current-transaction*) 'anonymous-transaction))

(defmethod encode-object ((transaction anonymous-transaction) stream)
  (%write-tag #\N stream)
  (%encode-string (anonymous-transaction-label transaction) stream)
  (let ((subtxns (flex:get-output-stream-sequence (anonymous-transaction-log-buffer transaction))))
    (%encode-integer (length subtxns) stream)
    (write-sequence subtxns stream)))

(defmethod decode-object ((tag (eql #\G)) stream)
  (make-instance 'anonymous-transaction
                 :transactions (%decode-list stream)))

(defvar *txn-log-stream* nil
  "This variable is bound to the transaction log stream while loading
   the transaction log.  It is used by anonymous transactions to read
   the subtransactions from the log.")

(defmethod decode-object ((tag (eql #\N)) stream)
  (let* ((label (%decode-string stream))
         (length (%decode-integer stream))
         (buffer (make-array length :element-type '(unsigned-byte 8))))
    (read-sequence buffer stream)
    (make-instance 'anonymous-transaction
                   :label label
                   :log-buffer (flex:make-in-memory-input-stream buffer))))

(define-condition rollback-failed (error)
  ((transaction :initarg :transaction)
   (original-error :initarg :original-error))
  (:report (lambda (e stream)
             (with-slots (transaction original-error) e
               (format stream "Rollback of transaction ~A failed: ~A" transaction original-error)))))

(defun anonymous-transaction-undo (transaction)
  (handler-case
      (dolist (command (anonymous-transaction-undo-log transaction))
        (apply (car command) (cdr command)))
    (error (e)
      (error 'rollback-failed
             :transaction transaction
             :original-error e))))

(defun do-with-transaction (label thunk)
  (with-store-guard ()
    (funcall thunk)))

(defmacro with-transaction ((&optional label) &body body)
  `(do-with-transaction ,(if (symbolp label) (symbol-name label) label)
     (lambda () ,@body)))

(defmethod execute-unlogged ((transaction anonymous-transaction))
  ;; EXECUTE-UNLOGGED is called for anonymous transactions only when
  ;; restoring from the transaction log.  It reads and executes the
  ;; subtransactions from the transaction log.
  (assert (eq :restore (store-state *store*)) ()
          "Unexpected store state ~A for EXECUTE-UNLOGGED on an anonymous transaction" (store-state *store*))
  (let ((stream (anonymous-transaction-log-buffer transaction)))
    (handler-case
        (loop
           (execute-unlogged (decode stream)))
      (end-of-file ()))))

(defmethod execute-transaction :before ((executor anonymous-transaction) transaction)
  (encode transaction (anonymous-transaction-log-buffer executor)))

;;; Subsystems

(defgeneric snapshot-subsystem (store subsystem))
(defgeneric close-subsystem (store subsystem))

(defmethod close-subsystem ((store store) (subsystem t)))

(defun snapshot ()
  (snapshot-store *store*))

(defun make-backup-directory (store)
  "Create directory pathname to place backup for STORE in.  By
default, the current time stamp is used.  If that directory already
exists, attach a dot and an incrementing number to the directory
pathname until a non-existant directory name has been found."
  (loop with timetag = (timetag)
     for i = nil then (if i (incf i) 1)
     for directory = (merge-pathnames (make-pathname :directory (list :relative (format nil "~A~@[.~A~]" timetag i)))
                                      (store-directory store))
     unless (probe-file directory)
     return directory))

(defmethod snapshot-store ((store store))
  (unless (store-open-p)
    (error 'store-not-open))
  (when (null (store-subsystems store))
    (error 'no-subsystems))
  (ensure-store-current-directory store)
  (with-store-state (:read-only store)
    (with-store-guard ()
      (with-log-guard ()
        (let ((backup-directory (make-backup-directory store)))
          (close-transaction-log-stream store)

          ;; CMUCL will, dass das directory existiert, ACL nicht
          #+(or cmu sbcl)
          (ensure-directories-exist backup-directory)

          (when *store-debug*
            (warn "Backup of the datastore in ~A."
                  backup-directory))
          (rename-file (store-current-directory store) backup-directory)
          (ensure-store-current-directory store)

          (let ((error t))
            (unwind-protect
                 (with-store-state (:snapshot)
                   (update-store-random-state store)
                   (dolist (subsystem (store-subsystems store))
                     (when *store-debug*
                       (report-progress "Snapshotting subsystem ~A of ~A~%" subsystem store))
                     (snapshot-subsystem store subsystem)
                     (when *store-debug*
                       (report-progress "Successfully snapshotted ~A of ~A~%" subsystem store)))
                   (setf (store-transaction-run-time store) 0)
                   (setf error nil))
              (when error
                (warn "Restoring backup ~A to current." backup-directory)
                (rename-file backup-directory (store-current-directory store))))))))))

(defvar *show-transactions* nil)

#+ (and lispworks (or linux darwin))
(cffi:defcfun (ffi-truncate "truncate") :int
  (path :string)
  ;; this next one is off_t, which is __darwin_off_t on Mac, and
  ;; __kernel_off_t on Linux. On Darwin that is int64, on Linux that is long.
  ;; (which should both be the same on 64 bit systems, but being careful anyway).
  ;; https://opensource.apple.com/source/xnu/xnu-1504.9.17/bsd/sys/_types.h.auto.html
  ;;
  (pos #+darwin :int64 #+linux :long))

(defun truncate-log (pathname position)
  (let ((backup (make-pathname :type (format nil "backup-~a-~a-~a"
                                             (get-universal-time)
                                             position
                                             (random 1000))
                               :defaults pathname)))
    (warn "Truncating transaction-log file ~a to ~a" pathname position)
    (report-progress "~&; creating log file backup: ~A~%" backup)
    (with-open-file (s pathname
                       :element-type '(unsigned-byte 8)
                       :direction :input)
      (with-open-file (r backup
                         :element-type '(unsigned-byte 8)
                         :direction :output)
        (copy-stream s r))))
  (report-progress "~&; truncating transaction log at position ~D.~%" position)
  #+cmu
  (unix:unix-truncate (ext:unix-namestring pathname) position)
  #+sbcl
  (sb-posix:truncate (namestring pathname) position)
  #+(or openmcl (and lispworks (or linux darwin)))
  (ffi-truncate (namestring pathname) position)
  #-(or cmu sbcl openmcl (and lispworks (or linux darwin)))
  (error "don't know how to truncate files on this platform"))

(defun load-transaction-log (pathname &key until)
  (let (length position txn)
    (restart-case
        (with-open-file (s pathname
                           :element-type '(unsigned-byte 8)
                           :direction :input)
          (setf length (file-length s))
          (loop
             (setf position (file-position s))
             (unless (< position length)
               (return))
             (setf txn (decode s))
             (cond
               ((and until
                     (> (transaction-timestamp txn) until))
                (truncate-log pathname position)
                (return-from load-transaction-log))
               (t
                (when *show-transactions*
                  (report-progress "~&;;; ~A txn @~D: ~A~%" (transaction-timestamp txn) position txn))
                (let ((*txn-log-stream* s))
                  (execute-unlogged txn))))))
      (discard ()
        :report (lambda (stream) (format stream "Discard rest of the transaction log" txn))
        (truncate-log pathname position)))))

(defgeneric restore-subsystem (store subsystem &key until))

(defun restore (&optional until)
  (restore-store *store* :until until))


(defmethod restore-transaction-log ((store store) transaction-log &key until)
  (when (probe-file transaction-log)
    (report-progress "loading transaction log ~A~%" transaction-log)
    (setf (store-transaction-run-time store) 0)
    (load-transaction-log transaction-log :until until)))

(defmethod restore-store ((store store) &key until)
  (ensure-store-random-state store)
  (report-progress "restoring ~A~%" store)
  (let ((*store* store))
    (setf (store-state store) :opened)
    (with-store-state (:restore)
      (with-store-guard ()
        (with-log-guard ()
          (close-transaction-log-stream store)
          (let ((transaction-log (store-transaction-log-pathname store))
                (error t))
            ;; restore the subsystems
            (unwind-protect
                 (progn
                   ;; Subsystems may not do any persistent changes when restoring.
                   (dolist (subsystem (store-subsystems store))
                     ;; check that UNTIL > snapshot date
                     (when *store-debug*
                       (report-progress "Restoring the subsystem ~A of ~A~%" subsystem store))
                     (restore-subsystem store subsystem :until until))
                   (restore-transaction-log store transaction-log :until until)
                   (setf error nil))
              (when error
                (dolist (subsystem (store-subsystems store))
                  (when *store-debug*
                    (report-progress "Closing the subsystem ~A of ~A~%"
                                     subsystem store))
                  (close-subsystem store subsystem)
                  (setf (store-state store) :closed))))))))))

#|
(defmacro disallow-cl-function-in-transaction (function)
  `(defun ,function (&rest args)
     (when (in-transaction-p)
       (error 'invalid-environment-access :function ',function))
     (apply (find-symbol ,(symbol-name function) :common-lisp) args)))

(disallow-cl-function-in-transaction get-internal-run-time)
(disallow-cl-function-in-transaction get-internal-real-time)
(disallow-cl-function-in-transaction sleep)

(defun get-universal-time ()
  (if (in-transaction-p)
      (transaction-timestamp *current-transaction*)
      (common-lisp::get-universal-time)))
|#
