;; -*- mode: lisp; syntax: common-lisp -*-

(in-package :libssh2)

(defparameter *ssh-connection* nil
  "Dynamic variable which holds an instance of SSH-CONNECTION which is
  a wrapper around the libssh2 'session' pointer, and contains
  additional information about host, port etc.")

(defparameter *sftp-session* nil
  "Dynamic variable which is bound to a foreign reference representing
  the SFTP session (in libssh2 terms).")

(defmacro result-or-error (&body body)
  `(let* ((results (multiple-value-list (progn ,@body)))
          (throwable-errors *errors-list*)
          (result (car results))
          (result-keyword (typecase result
                            (keyword result)
                            (integer (foreign-enum-keyword '+ERROR-CODE+ result :errorp nil))
                            (t (session-last-errno (session *ssh-connection*))))))
     (ssh2.dribble "Result of ~A: ~A keyword: ~A" ',(caar body) results result-keyword)
     (if (find result-keyword throwable-errors)
         (error 'ssh-generic-error
                 :code result-keyword
                 :message (if (eql result-keyword :error-sftp-protocol)
                              (libssh2-sftp-last-error *sftp-session*)
                              (session-last-error (session *ssh-connection*))))
         (values-list results))))

(defun print-memory (addr size)
  (format t "~{~x ~}"
          (loop for i below size
                collect (mem-aref addr :unsigned-char i))))

(define-foreign-library libssh2
  (:darwin "libssh2.dylib")
  (:unix  "libssh2.so.1")
  (:win32 "libssh2-1.dll")
  (t (:default "libssh2")))

(use-foreign-library libssh2)

(defcfun ("libssh2_init" %library-init) +ERROR-CODE+)
(defun library-init ()
  (result-or-error
    (%library-init)))

(defcfun ("libssh2_version" %library-version) :string
  (required :int))

(defcfun ("libssh2_exit" library-exit) :void)

(defcfun ("libssh2_session_init_ex" session-init-ex) +session+
  (alloc :pointer) (free :pointer) (realloc :pointer) (abstract :pointer))
(defcfun ("libssh2_session_free" %session-free) +ERROR-CODE+
  (session +session+))
(defun session-free (session)
  (%session-free session))

(defcfun ("libssh2_session_last_error" %session-last-error) +ERROR-CODE+
  (session +session+)
  (error-message :pointer) (error-message-buffer-size :pointer)
  (ownership :int))

(defun session-last-error (session)
  (with-foreign-objects ((fo-error-message-buffer-ptr   :pointer 1)
                         (fo-error-message-buffer-size  :int     1))
    (let ((retval (%session-last-error session
                                       fo-error-message-buffer-ptr
                                       fo-error-message-buffer-size
                                       0)))
      (let ((error-message-ptr  (mem-aref fo-error-message-buffer-ptr :pointer 0)))
        (values-list (list (convert-from-foreign error-message-ptr :string)
                           retval))))))


(defcfun ("libssh2_session_last_errno" session-last-errno) +ERROR-CODE+
  (session +session+))

(defcfun ("libssh2_trace" library-trace) :void
  (session +session+) (options +TRACE-OPTIONS+))

(defcfun ("libssh2_session_set_blocking" session-set-blocking) :void
  (session +session+) (blocking +BLOCKING+))

(defun session-init ()
  (let ((session (session-init-ex (null-pointer)
                                  (null-pointer)
                                  (null-pointer)
                                  (null-pointer))))
    (if (null-pointer-p session)
        (error 'ssh-generic-error :code :UNKNOWN :message "Could not initialise a session object with session-init-ex")
        (progn
          (session-set-blocking session :NON-BLOCKING)
          session))))

(defcfun ("libssh2_session_disconnect_ex" %session-disconnect) +ERROR-CODE+
  (session +session+) (reason +DISCONNECT-CODE+) (description :string) (lang :string))

(defun session-disconnect (session &key
                           (reason :AUTH-CANCELLED-BY-USER)
                           (description "")
                           (lang ""))
  (with-foreign-strings ((fs-description description)
                         (fs-lang        lang))
    (result-or-error
      (%session-disconnect session reason fs-description fs-lang))))

(defmacro with-session ( (session) &body body )
  `(let ((,session (session-init)))
     (unwind-protect
          (progn
            ,@body)
       (session-free ,session))))

(if (foreign-symbol-pointer "libssh2_session_handshake")
    (defcfun ("libssh2_session_handshake" %session-handshake) +ERROR-CODE+
      (session +session+) (socket :int))
    (defcfun ("libssh2_session_startup" %session-handshake) +ERROR-CODE+
      (session +session+) (socket :int)))

(defun session-handshake (session socket)
  (result-or-error
    (%session-handshake session socket)))

(defcfun ("libssh2_userauth_list" %session-auth-methods-list) :string
  (session +session+) (username :string) (username-length :unsigned-int))

(defun session-auth-methods-list (session username)
  (with-foreign-string ((fs-username fs-username-size) username)
    (let ((result  (%session-auth-methods-list
                    session fs-username (- fs-username-size 1))))
      (if result
          (mapcar (lambda (item) (intern (string-upcase item) 'keyword))
                  (split-sequence:split-sequence
                   #\, result))
          (result-or-error
            (session-last-errno session))))))

(defcfun ("libssh2_agent_init" %agent-init) +ssh-agent+
  (session +session+))

(defmacro with-agent ((agent session) &body body)
  `(let ((,agent (agent-init ,session)))
     (unwind-protect
          (progn ,@body)
       (unless (null-pointer-p ,agent)
         (agent-free ,agent)))))

(defun agent-init (session)
  (let ((agent (%agent-init session)))
    (if (null-pointer-p agent)
        (result-or-error
          (session-last-errno session))
        agent)))

(defcfun ("libssh2_agent_free" agent-free) :void
  (agent +ssh-agent+))

(defcfun ("libssh2_agent_connect" %agent-connect) +ERROR-CODE+
  (agent +ssh-agent+))
(defun agent-connect (agent)
  (result-or-error
    (%agent-connect agent)))

(defcfun ("libssh2_agent_disconnect" %agent-disconnect) +ERROR-CODE+
  (agent +ssh-agent+))
(defun agent-disconnect (agent)
  (result-or-error
    (%agent-disconnect agent)))

(defcfun ("libssh2_agent_list_identities" %agent-list-identies) +ERROR-CODE+
  (agent +ssh-agent+))
(defun agent-list-identies (agent)
  (result-or-error
    (%agent-list-identies agent)))

(defcfun ("libssh2_agent_get_identity" %agent-get-identity) +IDENTITY-AMOUNT+
  (agent +ssh-agent+)
  (store :pointer) (previous-public-key :pointer))

(defun agent-identities-iterator (agent)
  (when (eq  (agent-list-identies agent) :ERROR-NONE)
    (let ((agent agent)
          (prev  (null-pointer)))
      (lambda ()
        (with-foreign-object (store :pointer)
          (unless (eq (%agent-get-identity agent store prev)
                      :END)
            (setf prev
                  (mem-aref store :pointer 0))))))))

(defmacro foreach-agent-identity ((identy agent) &body body)
  `(let ((agent ,agent)
         (list-identies (agent-list-indenties ,agent))
         (prev (null-pointer)))
     (if (eq list-identies :ERROR-NONE)
         (with-foreign-object (store :pointer)
           (labels
               ((process-next-identity ()
                  (unless (eq (--agent-get-identity agent store prev)
                              :END)
                    (let ((,identy (setf prev
                                         (mem-aref store :pointer 0))))
                      ,@body
                      (process-next-identity)))))
             (process-next-identity))))))

(defcfun ("libssh2_knownhost_init" %known-hosts-init) +known-hosts+
  (session +session+))
(defun known-hosts-init (session)
  (let ((known-hosts (%known-hosts-init session)))
    (if (null-pointer-p known-hosts)
        (result-or-error
          (session-last-errno session))
        known-hosts)))

(defcfun ("libssh2_knownhost_free" known-hosts-free) :void
  (known-hosts +known-hosts+))

(defcfun ("libssh2_knownhost_readfile" %known-hosts-readfile) :int
  (known-hosts +known-hosts+) (filename :string) (type :int))

(defcfun ("libssh2_knownhost_writefile" %known-hosts-writefile) :int
  (known-hosts +known-hosts+) (filename :string) (type :int))

(defun known-hosts-readfile (hosts file)
  (with-foreign-string (foreign-file file)
    (let ((ret (%known-hosts-readfile hosts foreign-file 1)))
      (if (>= ret 0)
          (convert-from-foreign 0 '+ERROR-CODE+)
          (result-or-error
           (convert-from-foreign ret '+ERROR-CODE+))))))

(defun known-hosts-writefile (hosts file)
  (with-foreign-string (foreign-file file)
    (let ((ret (%known-hosts-writefile hosts foreign-file 1)))
      (if (>= ret 0)
          (convert-from-foreign 0 '+ERROR-CODE+)
          (result-or-error
            (convert-from-foreign ret '+ERROR-CODE+))))))

(defcfun ("libssh2_session_hostkey" %session-hostkey)  +key+
  (session +session+) (len :pointer) (type :pointer))

(defun session-hostkey (session)
  (with-foreign-objects ((len :unsigned-int 1)
                        (type :int 1))
    (let ((result (%session-hostkey session len type)))
      (make-key :data result
                :size (mem-aref len :long 0)
                :type (mem-aref type :int 0)))))

(defcfun ("libssh2_hostkey_hash" session-hostkey-hash) +keyhash+
  (session +session+) (hash-type +HASH-TYPE+))

(defun session-hostkey-fingerprint (session &optional (type :SHA1))
  (let ((hash (session-hostkey-hash session type)))
    (format nil "~{~2,'0X~^:~}"
            (loop for i below (if (eq type :SHA1) 20 16)
               collect (mem-aref hash :unsigned-char i)))))

(defcfun ("libssh2_knownhost_checkp" %known-hosts-checkp) +CHECK-VERDICT+
  (known-hosts +known-hosts+) (hostname :string) (port :int)
  (key +key+) (key-data-size :unsigned-int)
  (type :int)  (known-host :pointer))

(defcfun ("libssh2_knownhost_check" %known-hosts-check) +CHECK-VERDICT+
  (known-hosts +known-hosts+) (hostname :string)
  (key +key+) (key-data-size :unsigned-int)
  (type :int)  (known-host :pointer))

(defun known-hosts-check (known-hosts hostname key
                          &key
                            (port nil)
                            (flags '(.type-plain. .raw.))
                            (known-host (null-pointer)))
  (let ((fp (key-data key)))
    (if (null-pointer-p fp)
        (error 'ssh-generic-error :code :UNKNOWN :message "Host key is null")
        (with-foreign-string (fs-hostname hostname)
          (with-foreign-object (hostinfo :pointer 1)
            (setf (mem-aref hostinfo :pointer 0) known-host)
            (if port
                (%known-hosts-checkp known-hosts fs-hostname port
                                     fp
                                     (key-size key)
                                     (foreign-bitfield-value '+known-hosts-flags+ flags)
                                     hostinfo)
                (%known-hosts-check known-hosts fs-hostname
                                    fp
                                    (key-size key)
                                    (foreign-bitfield-value '+known-hosts-flags+ flags)
                                    hostinfo)))))))

(define-condition known-hosts-reading-error (ssh-generic-error)
  ((file :type     string
         :initarg  :file
         :accessor file)))

(defmethod print-object :after ((khre known-hosts-reading-error) stream)
  (format stream "// ~a" (file khre)))

(defmacro with-known-hosts ( ( known-hosts (session known-hosts-filename)) &body body)
  `(let ((,known-hosts (known-hosts-init ,session))
         (*errors-list* (remove :ERROR-FILE *default-errors-list*)))
     (unwind-protect
          (if (and (not (null-pointer-p ,known-hosts))
                   (or (eq :error-none (known-hosts-readfile ,known-hosts ,known-hosts-filename))
                       (eq :error-none (known-hosts-writefile ,known-hosts ,known-hosts-filename))))
              (progn
                ,@body)
              (with-last-error (,session known-hosts-reading-error) :file ,known-hosts-filename)))
     (unless (null-pointer-p ,known-hosts)
       (known-hosts-free ,known-hosts))))

(defcfun ("libssh2_knownhost_addc" %known-hosts-addc) +ERROR-CODE+
  (known-hosts (:pointer (:struct +known-host+)))
  (host :string) (salt :string) (key :pointer) (key-length :unsigned-int)
  (comment :string) (comment-length :unsigned-int)
  (typemask :int) (known-host (:pointer (:struct +known-host+))))

(defun known-hosts-add (known-hosts host-full-string key
                        &key
                          (comment "")
                          (flags '(.type-plain. .raw. .ssh.))
                          (salt  "")
                          (store (null-pointer)))
  (if (and (not (null-pointer-p known-hosts))
           (not (null-pointer-p (key-data key)))
           (stringp host-full-string))
      (with-foreign-strings ((fs-host-full-string host-full-string)
                             (fs-salt     salt)
                             ((fs-comment fs-comment-size) comment))
        (result-or-error
          (%known-hosts-addc known-hosts
                             fs-host-full-string fs-salt
                             (key-data key) (key-size key)
                             fs-comment (- fs-comment-size 1)
                             (foreign-bitfield-value '+known-hosts-flags+ flags)
                             store)))))

(defcfun ("libssh2_agent_userauth" %agent-userauth) +ERROR-CODE+
  (agent +ssh-agent+) (username :string) (identity :pointer))

(defun user-auth-agent (agent username identity)
  (with-foreign-string (fs-username username)
    (result-or-error
      (%agent-userauth agent fs-username identity))))

(defcfun ("libssh2_userauth_password_ex" %user-auth-password) +ERROR-CODE+
  (session +session+)
  (username :string) (username-length :unsigned-int)
  (password :string) (password-length :unsigned-int)
  (password-change :pointer))

(defun user-auth-password (session username password &optional (callback (null-pointer)))
  (with-foreign-strings (((fs-username fs-username-size) username)
                         ((fs-password fs-password-size) password))
    (result-or-error
      (%user-auth-password session
                           fs-username (- fs-username-size 1)
                           fs-password (- fs-password-size 1)
                           callback))))

(defcfun ("libssh2_userauth_keyboard_interactive_ex" %user-auth-interactive) +ERROR-CODE+
  (session +session+)
  (username :string) (username-length :unsigned-int)
  (callback :pointer))

(defun user-auth-interactive (session username callback)
  (with-foreign-string ((fs-username fs-username-size) username)
    (%user-auth-interactive session
                            fs-username
                            (- fs-username-size 1)
                            callback)))

(defvar *keyboard-interactive-password* "")
(defcallback trivial-keyboard-interactive-emulation :void
    ((login :pointer)      (login-length       :unsigned-int)
     (instruction :string) (instruction-length :unsigned-int)
     (num-prompts :int)
     (prompts   (:pointer (:struct +kbd-prompt+)))
     (responses (:pointer (:struct +kbd-response+)))
     (abstract  (:pointer :pointer)))
  ;; Just don't care about input. Only send password
  ;; Please, write you'r own callback, if you care
  (declare
   (ignore login)       (ignore login-length)
   (ignore instruction) (ignore instruction-length)
   (ignore prompts)     (ignore abstract))
  (loop for i below num-prompts
     do
       (with-foreign-slots ((text length)
                            (mem-aref responses '+kbd-response+ i)
                            +kbd-response+)
         (setf text   (foreign-string-alloc *keyboard-interactive-password*))
         (setf length (foreign-funcall "strlen" :pointer text :unsigned-int)))))

(defun user-auth-interactive-trivial (session username password)
  (let ((*keyboard-interactive-password* password))
    (user-auth-interactive session username
                           (callback trivial-keyboard-interactive-emulation))))

(defcfun ("libssh2_userauth_publickey_fromfile_ex" %user-auth-publickey) +ERROR-CODE+
  (session +session+)
  (username :string) (username-len :unsigned-int)
  (public-key :string)
  (private-key :string) (password :string))

(defun user-auth-publickey (session username public-key private-key password)
  (with-foreign-strings (((fs-username fs-username-size) username)
                         (fs-public-key  public-key)
                         (fs-private-key private-key)
                         (fs-password    password))
    (result-or-error
      (%user-auth-publickey session fs-username (- fs-username-size 1)
                            fs-public-key fs-private-key fs-password))))

(defcfun ("libssh2_channel_open_ex" %channel-open-ex) +channel+
  (session +session+) (channel-type :string) (channel-type-length :unsigned-int)
  (window-size :unsigned-int) (packet-size :unsigned-int)
  (message :string) (message-length :unsigned-int))

(defun channel-open (session &key (channel-type "session")
                               (window-size 262144)
                               (packet-size 32768)
                               (message ""))
  (loop while t do
   (with-foreign-strings (((fs-channel-type fs-channel-type-size) channel-type)
                          ((fs-message      fs-message-size)      message))
     (let* ((pass-message (if (string= message "")
                              (null-pointer)
                              fs-message))
            (pass-message-size (if (string= message "")
                                   0
                                   (- fs-message-size 1)))
            (new-channel
              (%channel-open-ex session
                                fs-channel-type (- fs-channel-type-size 1)
                                window-size packet-size
                                pass-message
                                pass-message-size)))
       (cond
         ((not (null-pointer-p new-channel))
          (return new-channel))
         (t
          (let ((err (session-last-errno session)))
            (case err
              (:error-eagain
               (wait-for-fd-output session))
              (otherwise
               (return
                 (result-or-error (identity err))))))))))))

(defcfun ("libssh2_channel_close" %channel-close) +ERROR-CODE+
  (channel +channel+))
(defun channel-close (channel)
  (result-or-error
    (%channel-close channel)))

(defcfun ("libssh2_channel_free" %channel-free) +ERROR-CODE+
  (channel +channel+))

(defun channel-free (channel)
  (loop while t do
    (let ((err (%channel-free channel)))
      (cond
        ((eql :error-eagain err)
         (log:trace "wait-for-fd channel-free")
         (wait-for-fd-output channel))
       (t
        (return (result-or-error
                  (identity err))))))))

(defcfun ("libssh2_channel_wait_closed" %channel-wait-closed) +ERROR-CODE+
  (channel +channel+))

(defun channel-wait-closed (channel)
  (loop while t do
    (let ((err (%channel-wait-closed channel)))
      (cond
        ((eql :error-eagain err)
         (log:trace "wait-for-fd channel-wait-closed")
         (wait-for-fd-output channel))
        (t
         (return (result-or-error
                   (identity err))))))))

(defcfun ("libssh2_channel_wait_eof" %channel-wait-eof) +ERROR-CODE+
  (channel +channel+))
(defun channel-wait-eof (channel)
  (loop while t do
    (let ((err (%channel-wait-eof channel)))
      (case err
        (:error-eagain
         (log:trace "wait-for-fd channel-wait-eof")
         (wait-for-fd channel))
        (otherwise
         (return
          (result-or-error (identity err))))))))


(defcfun ("libssh2_channel_process_startup" %channel-process-startup) +ERROR-CODE+
  (channel +channel+)
  (request :string) (request-length :unsigned-int)
  (message :string) (message-length :unsigned-int))

(defcfun ("libssh2_channel_setenv_ex" %channel-setenv-ex) +ERROR-CODE+
  (channel +channel+)
  (varname :string) (varname-len :int)
  (value :string) (value-len :int))

(defun channel-setenv (channel name value)
  (with-foreign-strings (((fs-name  fs-name-size)  name)
                         ((fs-value fs-value-size) value))
    (result-or-error
      (%channel-setenv-ex channel
                          fs-name  (- fs-name-size 1)
                          fs-value (- fs-value-size 1)))))

(defun channel-process-start (channel request message)
  (loop while t do
   (with-foreign-strings (((fs-request fs-request-size) request)
                          ((fs-message fs-message-size) message))
     (let ((err (%channel-process-startup channel
                                          fs-request (- fs-request-size 1)
                                          fs-message (- fs-message-size 1))))
       (case err
         (:error-eagain
          (wait-for-fd-output channel))
         (otherwise
          (return
           (result-or-error
             (identity err)))))))))

(defun channel-exec (channel cmd)
  (channel-process-start channel "exec" cmd))

(defun channel-shell (channel cmd)
  (channel-process-start channel "shell" cmd))

(defun channel-subsystem (channel cmd)
  (channel-process-start channel "subsystem" cmd))

(defcfun ("libssh2_channel_read_ex" %channel-read-ex) :int
  (channel +CHANNEL+) (stream +STREAM-ID+)
  (buffer :pointer) (buffer-length :unsigned-int))

(defcfun ("libssh2_channel_flush_ex" %channel-flush-ex) :int
  (channel +CHANNEL+) (stream +STREAM-ID+))

(defun channel-flush (channel)
  (loop while t do
    (let ((ret (%channel-flush-ex channel :ALL)))
      (cond
        ((> ret 0)
         (return :ERROR-NONE))
        ((eql +eagain+ ret)
         (wait-for-fd-output channel))
        (t
         (return
          (result-or-error
            (convert-from-foreign ret '+ERROR-CODE+))))))))

(defvar *channel-read-type* :STDOUT)
(defvar *channel-read-zero-as-eof* nil)
(defun channel-read (channel output-buffer &key (start 0) (end nil) (type *channel-read-type*)
                                             (non-blocking nil))
  (with-pointer-to-vector-data (buffer output-buffer)
    (loop while t do
      (let ((ret (%channel-read-ex channel type
                                   (inc-pointer buffer start)
                                   (if end
                                       (- (min end (length output-buffer))
                                          start)
                                       (- (length output-buffer)
                                          start)))))
        (cond
          ((>= ret 0)
            (return
              (values
               ret
               (cond
                 #+nil
                 ((and (= ret 0) *channel-read-zero-as-eof*) t)
                 ((= ret 0)
                  t
                  #+nil
                  (let ((eofp (channel-eofp channel)))
                    (log:info "Got eofp: ~a" eofp)
                    eofp))
                 (t nil)))))
          ((eql +eagain+ ret)
           (cond
             (non-blocking
              (return +eagain+))
             (t
              (wait-for-fd channel))))
          (t
           (return
             (result-or-error
               (convert-from-foreign ret '+ERROR-CODE+)))))))))

(defcfun ("libssh2_channel_write_ex" %channel-write-ex) :int
  (channel +CHANNEL+) (stream +STREAM-ID+)
  (buffer :pointer) (buffer-length :unsigned-int))

(defun wait-for-fd (channel)
  (declare (ignore channel))
  (log:trace "wait for fd called")
  (sleep 0.01))

(defun wait-for-fd-output (channel)
  (declare (ignore channel))
  (log:trace "wait for fd output called")
  (sleep 0.01))

(defmacro channel-write-with-conv (name conv)
  `(defun ,name (channel data &key (start 0) (end nil) (type *channel-read-type*))
     (,conv (buffer data)
            (loop while t do
              (let ((ret (%channel-write-ex channel type
                                            (inc-pointer buffer start)
                                            (if end
                                                (- (min end (length data))
                                                   start)
                                                (- (length data)
                                                   start)))))
                (cond
                  ((>= ret 0)
                   (log:trace "Wrote ~a bytes" ret)
                   (return ret))
                  ((eql +eagain+ ret)
                   (log:trace "Wait-for-fd in write-with-conv")
                   (wait-for-fd-output channel))
                  (t
                   (return
                    (result-or-error
                      (convert-from-foreign ret '+ERROR-CODE+))))))))))

(channel-write-with-conv channel-write with-pointer-to-vector-data)
(channel-write-with-conv channel-write-string with-foreign-string)

(defcfun ("libssh2_channel_send_eof" %channel-send-eof) +ERROR-CODE+
  (channel +channel+))

(defun channel-send-eof (channel)
  (loop while t do
    (let ((err (%channel-send-eof channel)))
      (case err
        (+eagain+
         (log:trace "wait-for-fd channel-send-eof")
         (wait-for-fd-output channel))
        (otherwise
         (return
          (result-or-error (identity err))))))))

(defcfun ("libssh2_channel_eof" %channel-eofp) +CHANNEL-EOF+
  (channel +channel+))
(defun channel-eofp (channel)
  (eq (%channel-eofp channel) :EOF))

(defcfun ("libssh2_channel_get_exit_status" channel-exit-status) :int
  (channel +channel+))

;; (defcfun ("libssh2_channel_get_exit_signal" --channel-exit-signal) +ERROR-CODE+
;;  (channel +channel+)

(defcfun ("libssh2_scp_recv" %scp-recv) +channel+
  (session +session+) (path :string) (stat +stat+))

(defun channel-scp-recv (session path)
  (with-foreign-string (fs-path path)
    (with-foreign-object (stat '+stat+ 1)
      (let ((result (%scp-recv session path stat)))
        (if (null-pointer-p result)
            (result-or-error
              (session-last-errno session))
            (progn
              (channel-send-eof result)
              (values result
                      (with-foreign-slots ((mode mtime atime) stat +stat+)
                        (list :mode  mode
                              :mtime mtime
                              :atime atime)))))))))

(defcfun ("libssh2_scp_send_ex" %scp-send-ex) +channel+
  (session +session+) (path :string) (mode :int) (size :unsigned-int)
  (mtime :long) (atime :long))

(defun get-universal-unix-time ()
  (- (get-universal-time)
     (encode-universal-time 0 0 0 1 1 1970 0)))

(defun channel-scp-send (session path size
                         &key mode mtime atime)
  (declare (optimize speed))
  (unless mode  (setq mode #b110100000))
  (unless mtime (setq mtime (get-universal-unix-time)))
  (unless atime (setq atime mtime))
  (with-foreign-string (fs-path path)
   (loop while t do
     (let ((result (%scp-send-ex session fs-path
                                 mode size mtime
                                 atime)))
       (if (null-pointer-p result)
           (let ((errno (session-last-errno session)))
             (case errno
               (:error-eagain
                (wait-for-fd session))
               (otherwise
                (return
                  (result-or-error
                    (session-last-errno session))))))
           (return result))))))

(defcfun ("libssh2_channel_direct_tcpip_ex" %channel-direct-tcpip-ex) +channel+
  (session +session+)
  (host :string)
  (port :int)
  (shost :string)
  (sport :int))


;;; SFTP related


(defmacro defcfun-error-check ((c-function-name lisp-function-name) result-type &rest rest)
  (let ((internal-name (alexandria:symbolicate "%" (symbol-name lisp-function-name)))
        (args (mapcar #'first rest)))
    `(progn
       (defcfun (,c-function-name ,internal-name) ,result-type
         ,@rest)
       (defun ,lisp-function-name ,args
         (result-or-error (,internal-name ,@args))))))

(defcstruct _libssh2-sftp-handle)

(defctype libssh2-sftp-handle (:pointer (:struct _libssh2-sftp-handle)))

(defctype size-t :unsigned-long)

(defctype libssh2-uint-64-t :unsigned-long-long)

(defcstruct _libssh2-sftp-attributes
  (flags :unsigned-long)
  (filesize libssh2-uint-64-t)
  (uid :unsigned-long)
  (gid :unsigned-long)
  (permissions :unsigned-long)
  (atime :unsigned-long)
  (mtime :unsigned-long))

(defctype libssh2-sftp-attributes (:pointer (:struct _libssh2-sftp-attributes)))

(defcfun-error-check ("libssh2_sftp_readdir_ex" libssh2-sftp-readdir-ex) :int
  (handle :pointer)
  (buffer (:pointer :char))
  (buffer-maxlen size-t)
  (longentry (:pointer :char))
  (longentry-maxlen size-t)
  (attrs libssh2-sftp-attributes))

(defcstruct _libssh2-sftp)

(defctype libssh2-sftp (:pointer (:struct _libssh2-sftp)))

(defcstruct _libssh2-sftp-statvfs
  (f-bsize libssh2-uint-64-t)
  (f-frsize libssh2-uint-64-t)
  (f-blocks libssh2-uint-64-t)
  (f-bfree libssh2-uint-64-t)
  (f-bavail libssh2-uint-64-t)
  (f-files libssh2-uint-64-t)
  (f-ffree libssh2-uint-64-t)
  (f-favail libssh2-uint-64-t)
  (f-fsid libssh2-uint-64-t)
  (f-flag libssh2-uint-64-t)
  (f-namemax libssh2-uint-64-t))

(defctype libssh2-sftp-statvfs (:pointer (:struct _libssh2-sftp-statvfs)))

(defcfun ("libssh2_sftp_statvfs" %libssh2-sftp-statvfs) :int
  (sftp :pointer)
  (path :string)
  (path-len size-t)
  (st :pointer))

(defun libssh2-sftp-statvfs (sftp path st)
  (with-foreign-strings (((fs-path fs-path-len) path))
    (result-or-error
      (%libssh2-sftp-statvfs sftp fs-path (- fs-path-len 1) st))))

(defcfun-error-check ("libssh2_sftp_read" libssh2-sftp-read) size-t
                     (handle :pointer)
                     (buffer (:pointer :char))
                     (buffer-maxlen size-t))

(defcfun ("libssh2_sftp_symlink_ex" %libssh2-sftp-symlink-ex) :int
  (sftp :pointer)
  (path :string)
  (path-len :unsigned-int)
  (target :string)
  (target-len :unsigned-int)
  (link-type :int))

(defun libssh2-sftp-symlink-ex (sftp source target link-type)
  (with-foreign-strings (((fs-source fs-source-len) source)
                         ((fs-target fs-target-len) target))
    (result-or-error
      (%libssh2-sftp-symlink-ex sftp fs-source (- fs-source-len 1) fs-target (- fs-target-len 1) link-type))))

(defcfun-error-check ("libssh2_sftp_shutdown" libssh2-sftp-shutdown) :int
                     (sftp :pointer))

(defcfun-error-check ("libssh2_sftp_close_handle" libssh2-sftp-close-handle) :int
                     (handle :pointer))

(defcfun-error-check ("libssh2_sftp_tell64" libssh2-sftp-tell-64) libssh2-uint-64-t
                     (handle :pointer))

(defcfun-error-check ("libssh2_sftp_write" libssh2-sftp-write) size-t
                     (handle :pointer) (buffer :pointer)
                     (count size-t))

(defcfun ("libssh2_sftp_rmdir_ex" %libssh2-sftp-rmdir-ex) :int
  (sftp :pointer)
  (path :string)
  (path-len :unsigned-int))

(defun libssh2-sftp-rmdir-ex (sftp path)
  (with-foreign-strings (((fs-path fs-path-len) path))
    (result-or-error
      (%libssh2-sftp-rmdir-ex sftp fs-path (- fs-path-len 1)))))

(defcfun ("libssh2_sftp_last_error" libssh2-sftp-last-error) sftp-error-code
  (sftp :pointer))

(defcfun ("libssh2_sftp_rename_ex" %libssh2-sftp-rename-ex) :int
  (sftp :pointer)
  (source-filename :string)
  (source-filename-len :unsigned-int)
  (dest-filename :string)
  (dest-filename-len :unsigned-int)
  (flags :long))

(defun libssh2-sftp-rename-ex (sftp source dest flags)
  (with-foreign-strings (((fs-source fs-source-len) source)
                         ((fs-dest fs-dest-len) dest))
    (result-or-error
      (%libssh2-sftp-rename-ex sftp fs-source (- fs-source-len 1) fs-dest (- fs-dest-len 1) flags))))

(defcfun-error-check ("libssh2_sftp_seek64" libssh2-sftp-seek-64) :void
  (handle :pointer)
  (offset libssh2-uint-64-t))

(defcfun-error-check ("libssh2_sftp_init" libssh2-sftp-init) :pointer
                     (session +session+))

(defcfun-error-check ("libssh2_sftp_seek" libssh2-sftp-seek) :void
                     (handle :pointer)
                     (offset size-t))

(defcfun-error-check ("libssh2_sftp_fstat_ex" libssh2-sftp-fstat-ex) :int
                     (handle :pointer)
                     (attrs :pointer)
                     (setstat :int))

(defcfun-error-check ("libssh2_sftp_get_channel" libssh2-sftp-get-channel) :pointer
                     (sftp :pointer))

(defcfun-error-check ("libssh2_sftp_fstatvfs" libssh2-sftp-fstatvfs) :int
                     (handle :pointer)
                     (st :pointer))

(defcfun ("libssh2_sftp_stat_ex" %libssh2-sftp-stat-ex) :int
  (sftp :pointer)
  (path :string)
  (path-len :unsigned-int)
  (stat-type :int)
  (attrs :pointer))

(defun libssh2-sftp-stat-ex (sftp path stat-type attrs)
  (with-foreign-strings (((fs-path fs-path-len) path))
    (result-or-error
      (%libssh2-sftp-stat-ex sftp fs-path (- fs-path-len 1) stat-type attrs))))

(defcfun-error-check ("libssh2_sftp_tell" libssh2-sftp-tell) size-t
                     (handle :pointer))

(defcfun ("libssh2_sftp_mkdir_ex" %libssh2-sftp-mkdir-ex) :int
  (sftp :pointer)
  (path :string)
  (path-len :unsigned-int)
  (mode :long))

(defun libssh2-sftp-mkdir-ex (sftp path mode)
  (with-foreign-strings (((fs-path fs-path-len) path))
    (result-or-error
      (%libssh2-sftp-mkdir-ex sftp fs-path (- fs-path-len 1) mode))))

(defcfun ("libssh2_sftp_unlink_ex" %libssh2-sftp-unlink-ex) :int
  (sftp :pointer)
  (filename :string)
  (filename-len :unsigned-int))

(defun libssh2-sftp-unlink-ex (sftp filename)
  (with-foreign-strings (((fs-filename fs-filename-len) filename))
    (result-or-error
      (%libssh2-sftp-unlink-ex sftp fs-filename (- fs-filename-len 1)))))

(defcfun ("libssh2_sftp_open_ex" %libssh2-sftp-open-ex) :pointer
  (sftp :pointer)
  (filename :string)
  (filename-len :unsigned-int)
  (flags :unsigned-long)
  (mode :long)
  (open-type sftp-open-types))

(defun libssh2-sftp-open-ex (sftp filename flags mode open-type)
  (with-foreign-strings (((fs-filename fs-filename-len) filename))
    (result-or-error
      (%libssh2-sftp-open-ex sftp fs-filename (- fs-filename-len 1) flags mode open-type))))
