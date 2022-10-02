;; Copyright 2018-Present Modern Interpreters Inc.
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(in-package :hunchentoot-extensions)

(defparameter *disable-sentry* nil)

(defvar *url-list* nil
  "alist of name to mapping of parse tree for url, for use with make-url")

(defclass url-handler ()
  ((parse-tree :initarg :parse-tree
               :reader url-handler-parse-tree)
   (plugin :initarg :plugin
           :initform nil)
   (request-args :initarg :request-args
                 :reader url-handler-request-args)))

(defmethod url-handler-prefix ((url-handler url-handler))
  "")

(defclass base-acceptor (hunchentoot:easy-acceptor)
  ((db-config :initarg :db-config
              :accessor acceptor-db-config)))

(defvar *logger* (log4cl:make-logger))

(defun trim-output (output)
  (let ((size 400))
   (cond
     ((< (length output) size)
      output)
     (t
      (format nil "~A <...trimmed...>"
       (str:substring 0 size output))))))

(defmethod hunchentoot:acceptor-log-message ((acceptor base-acceptor) log-level format-string &rest format-arguments)
  (let* ((output (apply 'format nil format-string format-arguments))
         (output (trim-output output)))
    (case log-level
      (:info (log:info output))
      (:error (log:error output))
      (:warning (log:warn output))
      (otherwise (log:info "Could not find log-level ~S" log-level)))))

;; parse tree format:
;; tree -> (:join tree1 tree2)
;; tree -> (:optional tree)
;; tree -> (:variable "VAR-NAME")
;; tree -> (:path "pathname")
;; tree -> (:regex-tree generic-regex-parse-tree)
;; tree -> (:function 'function-name)


(let ((regex "^/([^/(]*)([/(].*)?$"))
  (defun split-url-parts (url)
    (cl-ppcre:do-register-groups (first rest)
        (regex url)
      (return  (list
                first
                rest)))))

(defun %make-uri-parse-tree (uri)
  (assert (not (functionp uri)))
  (assert (not (equal "/" uri)))
  (cond
    (t
     (assert (str:starts-with-p "/" uri))
     (labels ((inner-regex (url)
                (declare (ignore uri))
                (cond
                  ((str:starts-with-p "(" url)
                   (assert (str:ends-with-p ")" url))
                   (let ((inner-str (str:substring 1 (1- (length url)) url)))
                    (multiple-value-bind (regex vars)
                        (inner-regex inner-str)
                      (values `(:optional ,regex)
                              vars))))
                  (t
                   (assert (str:starts-with-p "/" url))
                   (destructuring-bind (first-part &optional rest)
                       (split-url-parts url)
                     (declare (ignore unused))
                     (multiple-value-bind (first-regex first-vars)
                         (cond
                           ((str:starts-with-p ":" first-part)
                            (let ((variable-name (str:substring 1 nil first-part)))
                             (values
                              `(:variable ,variable-name)
                              (list (string-upcase variable-name)))))
                           (t
                            (values `(:path ,first-part) nil)))
                       (cond
                         (rest
                          (multiple-value-bind (inner-regex inner-vars)
                              (inner-regex rest)
                            (values `(:join
                                      ,first-regex
                                      ,inner-regex)
                                    (append first-vars inner-vars))))
                         (t
                          (values first-regex first-vars)))))))))
       (multiple-value-bind (regex vars) (inner-regex uri)
         (values regex vars))))))

(defun %make-uri-regex (tree)
  "Returns two values, a regex that matches the request, and a list of
  all the arguments."
  (log:debug "looking at: ~s" tree)
  (destructuring-bind (type val &optional val2) tree
    (ecase type
      (:path
       (assert (not val2))
       `(:sequence "/" ,val))
      (:variable
       `(:sequence "/" (:register (:greedy-repetition 1 nil (:inverted-char-class #\/)))))
      (:optional
       (assert (not val2))
       `(:greedy-repetition 0 1 ,(%make-uri-regex val)))
      (:join (list :sequence (%make-uri-regex val) (%make-uri-regex val2)))
      (:regex-tree `(:sequence :start-anchor ,val (:regex "/?") :end-anchor)))))

(defun make-uri-regex (uri)
  "Returns three values: a compiled regex, list of variables, and the parse-tree"
  (cond
    ((functionp uri)
     `(:function ,uri))
    ((equal "/" uri)
     (values (cl-ppcre:create-scanner "^/$")
             nil
              '(:root)))
    (t
     (multiple-value-bind (parse-tree vars) (%make-uri-parse-tree uri)
       (log:debug "Got parse tree: ~a" parse-tree)
       (let* ((regex-str (%make-uri-regex parse-tree))
              (regex-str `(:sequence :start-anchor ,regex-str (:regex "/?") :end-anchor)))
         (log:debug "Got regex-str: ~s" regex-str)
         (values (cl-ppcre:create-scanner regex-str) vars parse-tree regex-str))))))

(defgeneric acceptor-funcall-handler (acceptor fn)
  (:documentation "better-easy-handler will call this to funcall the handler implementation"))

(defun matches-regex (regex request &key script-name)
  (declare (optimize (debug 3) (speed 0)))
  (let ((script-name (or script-name (hunchentoot:script-name request))))
    (cond
      ((and (consp regex)
            (eql :function (car regex)))
       (funcall (cadr regex) request))
      (t
       (cl-ppcre:scan-to-strings regex script-name)))))

(defmacro better-easy-handler ((name &key uri method acceptor-names intern (html t)) params &body body)
  (unless uri
    ;; if we don't do this the whole site crashes.
    (setf uri "/nil"))

  (let ((name (or name (make-name uri method)))
        (regex (gensym "REGEX"))
        (vars (gensym "VARS"))
        (parse-tree (gensym "PARSE-TREE")))
    (multiple-value-bind (body decls) (uiop:parse-body body)
     (multiple-value-bind (full-regex full-var-list) (when (stringp uri) (make-uri-regex uri))
       (declare (ignore full-regex))
       ;; we call make-uri-regex twice, once for getting the variables
       ;; at macro time, and the second for use in the closure.
       (let ((acceptor-names (if (symbolp (eval acceptor-names)) `(list ,acceptor-names) acceptor-names)))
         `(eval-when (:compile-toplevel :load-toplevel :execute)
            (declare-handler ',name)
            (eval-when (:load-toplevel :execute)
             (multiple-value-bind (,regex ,vars ,parse-tree) (make-uri-regex ,uri)
               (declare (ignorable ,vars))
               (hunchentoot:define-easy-handler (,name :uri (%only-request-of-type ,regex ,method) :acceptor-names ,acceptor-names) ,params
                 ,@decls
                 (when ,intern
                   (%assert-is-intern))
                 ;; todo: double regexing, but then, the acceptor is
                 ;; regexing a hundred or so different urls, so perhaps it
                 ;; doesn't matter.
                 ,(when full-var-list
                    `(multiple-value-bind (res args)
                         (matches-regex ,regex hunchentoot:*request*)
                       (declare (ignore res))
                       ,@ (loop for v in full-var-list
                                for i from 0 to 100
                                appending
                                (loop for param in params
                                      if (equal v (symbol-name param))
                                        collect `(unless ,param (setf ,param (elt args ,i)))))))
                 (%easy-handler-wrap
                  (lambda ()
                    (progn ,@body)) :html ,html))
               (when ',name
                 (setf (alexandria:assoc-value *url-list* ',name)
                       (make-instance 'url-handler
                                      :parse-tree ,parse-tree
                                      :request-args (loop for p in ',params collect
                                                                            (if (listp p) (car p) p)))))))))))))


(defmethod acceptor-funcall-handler (acceptor fn)
  (funcall fn))

(defun make-name (uri method)
  ;; interned in current package!
  (intern (format nil "~a-~a" uri method) *package*))

(defun prod-request? ()
  (destructuring-bind (host port) (str:split ":" (hunchentoot:host))
    (and
     (equal port "443")
     (not (str:starts-with-p "staging." host)))))

(defun discard-condition-p (condition)
  #+sbcl (typep condition 'sb-int:broken-pipe)
  #-sbcl nil)

(defmethod log-crash-extras ((acceptor base-acceptor) condition)
  `(("url" .
           ,(hunchentoot:request-uri*))
    ("user-agent" . , (hunchentoot:header-in* :user-agent))))

(defmethod log-crash-extras :around ((acceptor base-acceptor) condition)
  (loop for (k . v) in (call-next-method)
        collect (cons k (format nil "~a" v))))

#-screenshotbot-oss
(defmethod hunchentoot:maybe-invoke-debugger :after (condition)
  (when (and hunchentoot:*catch-errors-p*
             (not *disable-sentry*)
             #+lispworks
             (not
              (or
               (typep condition 'comm:socket-error)
               (typep condition 'comm:socket-io-error)))
             (prod-request?))
    ;; There's an error in trivial-backtrace:map-backtrace in SBCL
    ;; if we don't set sb-debug:*stack-top-hint* to NIL
    (let (#+sbcl (sb-debug:*stack-top-hint* nil))
      (unless (discard-condition-p condition)
        (when (find-package :sentry-client)
          (let ((capture-exception (find-symbol "CAPTURE-EXCEPTION" :sentry-client)))
            (funcall capture-exception condition
                     :extras
                     (when (boundp 'hunchentoot:*acceptor*)
                       (log-crash-extras hunchentoot:*acceptor* condition)))))))))

(defmethod hunchentoot:acceptor-log-access ((acceptor base-acceptor) &key return-code)
  "Default method for access logging.  It logs the information to the
destination determined by (ACCEPTOR-ACCESS-LOG-DESTINATION ACCEPTOR)
\(unless that value is NIL) in a format that can be parsed by most
Apache log analysis tools.)"

  (log:info "~:[-~@[ (~A)~]~;~:*~A~@[ (~A)~]~] ~:[-~;~:*~A~] [~A] \"~A ~A~@[?~A~] ~A\" ~D ~:[-~;~:*~D~] \"~:[-~;~:*~A~]\" \"~:[-~;~:*~A~]\""
          (hunchentoot:remote-addr*)
          (hunchentoot:header-in* :x-forwarded-for)
          (hunchentoot:authorization)
          (hunchentoot::iso-time)
          (hunchentoot:request-method*)
          (hunchentoot:script-name*)
          (hunchentoot:query-string*)
          (hunchentoot:server-protocol*)
          return-code
          (hunchentoot:content-length*)
          (hunchentoot:referer)
          (hunchentoot:user-agent)))

(defun %easy-handler-wrap (body &key (html t))
  (let* ((acceptor (hunchentoot:request-acceptor hunchentoot:*request*))
         (body (lambda ()
                 (acceptor-funcall-handler acceptor body))))
    (cond
      (html
       (let ((res (funcall body)))
         (setf (hunchentoot:content-type*) "text/html; charset=utf-8"
               (hunchentoot:header-out :last-modified) (hunchentoot::rfc-1123-date (get-universal-time))
               (hunchentoot:header-out :accept-ranges) "bytes")

         (let ((out (hunchentoot:send-headers)))
           (when out ;; head requests will have a nil stream
             (markup:write-html-to-stream res (flexi-streams:make-flexi-stream out :external-format :utf8))
             (finish-output out))
           res)))
      (t
       (let ((res (funcall body)))
         (unless (or
                  hunchentoot::*headers-sent*)
          (assert (Stringp res)))
         res)))))

(defun socket-detached-p ()
  "In some cases, we might detach the socket to asynchronously respond to the request"
  (null hunchentoot::*close-hunchentoot-stream*))

(defun %only-request-of-type (uri type)
  (assert (member type '(nil :get :post :delete :put)))
  (lambda (request)
    (and (or (not type)
             (eq (hunchentoot:request-method request) type))
         (matches-regex uri request))))

(defun is-intern? ()
  (let ((remote-addr (hunchentoot:real-remote-addr)))
   (or
    (member remote-addr '("192.168.1.1" "127.0.0.1") :test #'equal)
    (str:starts-with? "10.1.10." remote-addr)
    (equal "172.58.38.219" remote-addr) ;; temporary, change when needed
    (str:starts-with? "2603:3024:e9c:b0" remote-addr))))

(defun %assert-is-intern ()
  (assert (is-intern? )))

(defun safe-domain ()
  (destructuring-bind (host &optional (port "80")) (str:split ":" (hunchentoot:host))
    (format nil "~a://~a"
            (if (equal "443" port) "https" "http")
            (if (or
                 (equal "443" port)
                 (equal "80" port))
                host
                (hunchentoot:host)))))


(defmethod safe-redirect (target &rest args)
  (let ((target (if (and target (not (equal "" target)))
                    target
                    (hunchentoot:script-name hunchentoot:*request*))))
    (let ((target (apply 'make-url target args)))
     (cond
       ((str:starts-with? "/" target)
        (let ((host (hunchentoot:host)))
          (destructuring-bind (host &optional (port "80")) (str:split ":" host)
            (let* ((port (parse-integer port))
                   (https (equal port 443)))
              (hunchentoot:redirect
               target
               :host host
               :port port
               :protocol (if https
                             :https
                             :http))))))
       (t
        (hunchentoot:redirect target))))))
