;; -*- mode: lisp; syntax: common-lisp -*-

(defpackage :libssh2
  (:use :cffi :cl :trivial-gray-streams)
  (:export ;; LIBSSH2 API
           :with-session
           :session-init
           :session-disconnect
           :session-handshake
           :session-last-error
           :session-auth-methods-list

           :with-agent
           :agent-init
           :agent-free
           :agent-connect
           :agent-disconnect
           :agent-list-identies
           :agent-identities-iterator
           :foreach-agent-identity

           :with-known-hosts
           :known-hosts-add
           :known-hosts-init
           :known-hosts-free
           :known-hosts-readfile
           :known-hosts-writefile
           :known-hosts-check
           :session-hostkey
           :session-hostkey-fingerprint

           :user-auth-agent
           :user-auth-password
           :user-auth-publickey

           :channel-open
           :channel-close
           :channel-free
           :channel-wait-closed
           :channel-setenv
           :channel-process-start
           :channel-exec
           :channel-shell
           :channel-subsystem
           :channel-flush
           :channel-read
           :channel-write
           :channel-write-string
           :channel-eofp
           :channel-send-eof
           :channel-exit-status
           :channel-scp-recv
           :+eagain+
           :wait-for-fd

           ;; SFTP
           :sftp-list-directory

           ;; STREAMS API // BLOCKING

           :AUTH-DATA
           :SSH-CONNECTION
           :SESSION
           :SOCKET
           :HOST
           :PORT
           :HOSTS-DB
           :AUTH-PASSED
           :SSH-HANDSHAKE-ERROR
           :SSH-BAD-HOSTKEY
           :REASON
           :HASH
           :CREATE-SSH-CONNECTION
           :DESTROY-SSH-CONNECTION
           :WITH-SSH-CONNECTION
           :SSH-SESSION-KEY
           :AUTH-PASSWORD
           :SSH-VERIFY-SESSION
           :AUTHENTICATION-METHODS
           :AUTHENTICATION
           :AUTH-PUBLICKEY
           :PUBLIC-KEY
           :PRIVATE-KEY
           :PASSWORD
           :AUTH-AGENT
           :MAKE-PUBLICKEY-AUTH
           :MAKE-AGENT-AUTH
           :MAKE-PASSWORD-AUTH
           :MAKE-PASSWORD-EMUL-AUTH
           :MAKE-AUTH-DATA
           :SSH-CHANNEL-STREAM
           :SSH-CHANNEL-STREAM-OUTPUT
           :SSH-CHANNEL-STREAM-INPUT
           :SSH-CHANNEL-STREAM-INPUT/OUTPUT
           :SSH-CHANNEL-EXEC
           :SSH-CHANNEL-RECV
           :SSH-CHANNEL-SEND
           :CHANNEL
           :INPUT-BUFFER
           :INPUT-SIZE
           :OUTPUT-BUFFER
           :OUTPUT-SIZE
           :INTPUT-POS
           :OUTPUT-POS
           :STREAM-ELEMENT-TYPE
           :OPEN-STREAM-P
           :STREAM-LISTEN
           :STREAM-READ-BYTE
           :STREAM-READ-SEQUENCE
           :STREAM-READ-LINE
           :STREAM-FORCE-OUTPUT
           :STREAM-FINISH-OUTPUT*
           :STREAM-FINISH-OUTPUT
           :STREAM-WRITE-BYTE
           :STREAM-WRITE-CHAR
           :STREAM-WRITE-SEQUENCE
           :CLOSE
           :EXECUTE
           :SCP-INPUT
           :SCP-OUTPUT
           :WITH-EXECUTE
           :WITH-EXECUTE*
           :WITH-SCP-INPUT
           :WITH-SCP-OUTPUT
           :scp-get
           :scp-put

           ;; CONDITIONS & SLOTS
           :KNOWN-HOSTS-READING-ERROR
           :HOST-NOT-ALLOWED-TO-CONNECT
           :+TRACE-OPTIONS+
           :+DISCONNECT-CODE+
           :+ERROR-CODE+
           :+BLOCKING+
           :+IDENTITY-AMOUNT+
           :+STREAM-ID+
           :+HASH-TYPE+
           :+CHECK-VERDICT+
           :+SESSION+
           :+KEY+
           :+SSH-AGENT+
           :+KNOWN-HOSTS+
           :+KEYHASH+
           :+CHANNEL+
           :+KNOWN-HOSTS-FLAGS+
           :+KNOWN-HOST+
           :KEY
           :SSH-GENERIC-ERROR
           :MESSAGE
           :CODE :FILE

           ;; Restarts
           :TRY-CREATE-FILE
           :ACCEPT
           :ACCEPT-ONCE
           :ACCEPT-ALWAYS
           :DROP

           ;; Dynamic customizations
           :*CHANNEL-READ-TYPE*
           :*CHANNEL-READ-ZERO-AS-EOF*
           :*ERRORS-LIST*
           :*DEFAULT-ERRORS-LIST*
           :channel-direct-tcpip)
    (:import-from #:hu.dwim.logger
                #:+dribble+
                #:+debug+
                #:+info+
                #:+warn+
                #:+error+
                #:+fatal+
                #:deflogger
                #:find-logger
                #:log-level))
