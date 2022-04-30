(in-package #:auth)

(defgeneric auth-get-user-class (acceptor)
  (:documentation "Gets the class used to store the user data"))

(defclass user-session-value (store-object)
  ((session-key-and-prop-key
    :accessor session-key-and-prop-key)
   (value
    :initarg :value
    :accessor value))
  (:metaclass persistent-class))

(defmethod initialize-instance :after ((uv user-session-value)
                                       &key session-key
                                         prop-key
                                         value)
  (declare (ignore value))
  (setf (session-key-and-prop-key uv)
        (cons session-key prop-key)))



(let ((cache (make-hash-table :test #'equal)))
  (defun find-user-session-value (session-key prop-key)
    "The reason we don't use a BKNR index for this is because the
index has a tendency to go stale. When the index does go stale,
sessions are one of the first objects created that can make the index
incorrect. We don't have a crazy number of sessions per instance that
this would be a problem."
    (let ((cache-key (cons session-key prop-key)))
      (util/misc:or-setf
       (gethash cache-key cache)
       (loop for user-session-value in (bknr.datastore:store-objects-with-class 'user-session-value)
             if (equal cache-key (session-key-and-prop-key user-session-value))
                return user-session-value)))))


(defclass user-session-transient ()
  ((session-key
    :reader %session-token
    :initarg :token)
   (domain
    :reader session-domain
    :initform (host-without-port))))

(defmethod session-key ((session user-session-transient))
  (cons (%session-token session)
        (session-domain session)))


(defvar *secure-cookie-p* t)

(defun ip-address-p (domain)
  (or
   (search "localhost" domain)
   (cl-ppcre:scan "\\.\\d*$" domain)))

(defun fix-cookie-domain (domain)
  (cond
    ((ip-address-p domain)
     domain)
    (t
     domain)))

(defun host-without-port ()
  (car (str:split ":" (host))))

(defun set-session-cookie (token &optional domain)
  (let ((domain (or domain (host-without-port))))
    (let ((domain (fix-cookie-domain domain)))
     (set-cookie "s" :value token :domain domain :expires (+ (get-universal-time) 1000000)
		             :path "/" :secure (and
                                        *secure-cookie-p*
                                        (string=
                                         "https"
                                         (hunchentoot:header-in* :x-forwarded-proto)))))))

(defun has-session? ()
  (let ((s (cookie-in "s")))
    (and s (not (equal s "")))))

(defun drop-session (&optional domain)
  (set-session-cookie "" domain))

(defun %current-session ()
  (let ((token (cookie-in "s")))
    (and
     token
     (not (equal "" token))
     (make-instance 'user-session-transient
                    :token token))))


(defvar *session-token-generator*)

(defvar *lock* (bt:make-lock "auth-lock"))

#+windows
(defun read-windows-seed ()
  (cl-store:restore (path:catfile (util:system-source-directory :auth) "dummy-init-key.out")))

(setf *session-token-generator* (session-token:make-generator
                                 #+windows
                                 :initial-seed
                                 #+windows
                                 (progn
                                   (log:warn "Using insecure seed, only use on Windows")
                                   (read-windows-seed))))

(defun set-session (session &optional domain)
  (set-session-cookie (car (session-key session)) domain))

(defun set-session-user-id (user-id &optional domain)
  (assert user-id)
  (setf (session-value :user-id) user-id))

(defun %make-session ()
  "Only creates the session, does not do anything else with it"
  (let ((session (make-instance 'user-session-transient
				:token (funcall *session-token-generator*))))

    session))

(defvar *current-session*)

(defun session= (session1 session2)
  (and
   (string= (%session-token session1)
            (%session-token session2))
   (string= (session-domain session1)
            (session-domain session2))))

(defun %with-sessions (body)
  (let ((*current-session* (%current-session)))
    (unless *current-session*
      (setf *current-session* (%make-session))
      (set-session *current-session*)
      (assert *current-session*))
    (funcall body)))

(defmacro with-sessions (() &body body)
  "Inside of this macro CURRENT-SESSION will always return a non-nil
value."
  `(%with-sessions (lambda () ,@body)))

(defun current-session ()
  *current-session*)

(defun session-value (key &key (session (current-session)))
  (let ((x (find-user-session-value (session-key session)
                                    key)))
    (and x
         (value x))))


(defun (setf session-value) (value key &key (session (current-session)))
  (let ((x (bt:with-lock-held (*lock*)
             (or
              (find-user-session-value (session-key session)
                                       key)
              (make-instance 'user-session-value
                              :session-key (session-key session)
                              :prop-key key)))))
    (bknr.datastore:with-transaction ()
      (setf (value x) value))))

(defgeneric password-hash (user)
  (:documentation "password hash for the user"))


(defmethod check-password (user password)
  (and user
   (cl-pass:check-password password (auth:password-hash user))))

(defmethod (setf user-password) (password user)
  (setf (auth:password-hash user)
        (cl-pass:hash password)))

(defclass login-controller ()
  ((login-page
    :initarg :login-page)))
