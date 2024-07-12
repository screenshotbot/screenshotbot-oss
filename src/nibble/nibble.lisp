;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(in-package :nibble)

(markup:enable-reader)

(defvar *nibbles* (make-hash-table))
(defvar *named-nibbles* nil)

(defvar *lock* (bt:make-lock))
(defparameter *last-gc* 0)

(defclass nibble ()
  ((impl :initarg :impl)
   (id :initarg :id
       :reader nibble-id)
   (name :initarg :name
         :initform nil
         :reader nibble-name
         :documentation "A name use for analytics and debugging purposes")
   (args :initarg :args)
   (once :initarg :once
         :initform nil)
   (calledp :initform nil
            :accessor calledp)
   (acceptor :initarg :acceptor
             :initform nil
             :reader nibble-acceptor
             :documentation "The acceptor where this nibble was created from")
   (session :initarg :session)
   (user :initarg :user
         :initform nil
         :accessor nibble-user)
   (check-session-p :initarg :check-session-p
                    :initform nil)
   (ts :initarg :ts)))

(defun all-nibbles ()
  (bt:with-lock-held (*lock*)
    (loop for v being the hash-values of *nibbles* collect v)))

(defun gc (&key force)
  (let ((ts (get-universal-time)))
   (when (or force
             (> ts (+ *last-gc* 3600)))
     (setf *last-gc* ts)
     (let ((exp-time (- (get-universal-time)
                        (* 3 24 3600))))
       (loop for nib in (all-nibbles)
             if  (< (slot-value nib 'ts)
                    exp-time)
               do (bt:with-lock-held (*lock*)
                    (remhash (nibble-id nib) *nibbles*)))))))

(defclass nibble-acceptor-mixin ()
  ((nibble-prefix :initarg :nibble-prefix
                  :reader nibble-prefix))
  (:default-initargs :nibble-prefix "/n/"))

(defun make-id (ts)
  ;; We'll end up with a 120bit integer.
  (let ((entropy 88))
    (logior
     (ash (get-universal-time) entropy)
     (secure-random:number (ash 1 entropy)))))

(defun push-nibble (id nibble)
  (bt:with-lock-held (*lock*)
    (setf (gethash id *nibbles*)
          nibble)))

(defun get-nibble (id)
  (bt:with-lock-held (*lock*)
    (let ((nibble (gethash id *nibbles*)))
      nibble)))

(defun current-session ()
  (when (boundp 'hunchentoot:*request*)
   (auth:current-session)))

(defmacro nibble (args-and-options &body body)
  (cond
    ((and
      args-and-options
      (symbolp args-and-options))
     ;; this is a named-nibble
     (let ((args (assoc-value *named-nibbles* args-and-options)))
       `(nibble ()
          (funcall ',args-and-options ,@ (loop for arg in args
                                               appending
                                               (list (intern (string arg) "KEYWORD")
                                                     `(safe-parameter ',arg))))))
     )
    (t
     (let* ((position (position-if 'keywordp args-and-options))
            (args (subseq args-and-options 0 position))
            (options (when position (subseq args-and-options position))))
       (destructuring-bind (&key once name method (check-session-p t)) options
         (declare (ignore method)) ;; I don't remember why we introduced this
                                   ;; in the first place
         `(call-nibble :once ,once
                       :name ,name
                       :args ',args
                       :check-session-p ,check-session-p
                       :impl (lambda ,args ,@body)))))))

(defun call-nibble (&key once name args check-session-p impl)
  (let* ((ts (get-universal-time))
         (id (make-id ts))
         (session (current-session))
         (nibble (make-instance 'nibble
                                 :impl impl
                                 :name name
                                 :session (when session
                                            ;; In a test, we might not
                                            ;; always call
                                            ;; auth:with-sessions
                                            ;; Which will cause nibble
                                            ;; to fail. We could
                                            ;; potentially fix all the
                                            ;; tests to wrap with
                                            ;; auth:with-session, but
                                            ;; this works for now.
                                            (auth:ensure-session-created session))
                                 :acceptor (when (boundp 'hunchentoot:*acceptor*)
                                                    hunchentoot:*acceptor*)
                                 :user
                                 (cond
                                   ((boundp 'hunchentoot:*request*)
                                    (auth:request-user hunchentoot:*request*))
                                   (t
                                    ;; We're most likely running in tests. As a
                                    ;; safety measure, let's set it up
                                    ;; so that we'll never be able to
                                    ;; render this nibble again.
                                    (gensym "FAKE-NIBBLE-USER")))
                                 :check-session-p check-session-p
                                 :once once
                                 :args args
                                 :ts ts
                                 :id id)))
    (push-nibble id nibble)))

(defmethod hunchentoot:acceptor-dispatch-request ((self nibble-acceptor-mixin)
                                                  request)
  (cond
    ((str:starts-with-p (nibble-prefix self) (hunchentoot:script-name request))
     (render-nibble self (parse-integer
                          (third
                           (str:split "/" (hunchentoot:script-name request))))))
    (t
     (call-next-method))))

(defmethod render-nibble ((plugin nibble-acceptor-mixin) (nibble nibble))
  (render-nibble plugin (nibble-id nibble)))

(defun safe-parameter (arg)
  (hunchentoot:parameter (str:downcase arg)))

(defmethod nibble-render-logged-out (acceptor nibble)
  <html>
    <body>
      Please log back in to view this page.
      <a href= "/">Home</a>
    </body>
  </html>)

(define-condition expired-nibble (warning)
  ((name :initarg :name
         :initform nil)
   (src :initarg :src
        :initform nil))
  (:report (lambda (self stream)
             (with-slots (name src) self
               (format stream "Expired nibble with name ~a (from ~a) was accessed" name src)))))

(defmethod render-nibble ((plugin nibble-acceptor-mixin) (id string))
  (render-nibble plugin (parse-integer id)))

(defmethod nibble-funcall ((plugin nibble-acceptor-mixin) nibble)
  (with-slots (once impl args) nibble
    (when once
      ;; we need to make sure only one call of this nibble
      ;; happens.
      (bt:with-lock-held (*lock*)
        (when (calledp nibble)
          (error "Calling nibble multiple times"))
        (setf (calledp nibble) t)))
    (let ((args (loop for arg in args
                      collect (safe-parameter arg))))
      (apply impl args))))

(defmethod maybe-render-html (self)
  self)

(defmethod maybe-render-html ((self markup:abstract-xml-tag))
  (markup:write-html self))

(defmethod render-nibble ((plugin nibble-acceptor-mixin) (id number))
  (maybe-render-html
   (let ((nibble (get-nibble id)))
     (cond
       ((null nibble)
        (unless (str:containsp "Bytespider" (hunchentoot:user-agent))
          (warn 'expired-nibble
                :name (safe-parameter :_n)
                :src (safe-parameter :_src)))
        (setf (hunchentoot:return-code*) 410 #| GONE |# )
        (setf (hunchentoot:header-out :x-expired-nibble) "1")
        <html>
        <body>
        The page you're looking for has expired.
        <a href= "/">Go back</a>
        </body>
        </html>)
       (t
        (with-slots (session check-session-p) nibble
          (flet ((final-render ()
                   (nibble-funcall plugin nibble)))
            (cond
              ((and (boundp 'hunchentoot:*request*) check-session-p)
               ;; Before we call final-render, we should check
               ;; the session and logged in user.
               (let ((current-session (current-session)))
                 (cond
                   ((not (auth:session= session current-session))
                    (warn "Incorrect session: this is not a cause for concern unless it's spiking")
                    (render-incorrect-session plugin))
                   ((let ((nibble-user (nibble-user nibble)))
                      (and
                       ;; if the nibble was created when they
                       ;; weren't logged in, and they are logged
                       ;; in now, that's always okay
                       nibble-user
                       (not (eql (auth:request-user hunchentoot:*request*)
                                 nibble-user))))
                    (nibble-render-logged-out
                     hunchentoot:*acceptor*
                     nibble))
                   (t
                    (final-render)))))
              (t
               (final-render))))))))))

(defmethod render-incorrect-session ((plugin nibble-acceptor-mixin))
  <html>
    <body>
      <h1>You cannot view this page.</h1>
      <p>This URL is tied to a specific browser and user. You should not share this URL, or copy-and-paste
        it into another browser.</p>
      <a href= "/">Go back</a>
    </body>
  </html>)

(defun nibble-full-url (nibble)
  (hex:make-full-url
   hunchentoot:*request*
   (nibble-url nibble)))


(defun guess-src ()
  "The original root script that generated this nibble. This isn't
stored in the nibble itself, because we want this for debugging
purposes to be maintained across server restarts."
  (when (boundp 'hunchentoot:*request*)
    (let ((param (hunchentoot:parameter "_src")))
     (cond
       (param
        param)
       (t
        (hunchentoot:script-name*))))))

(defun nibble-url (nibble)
  (quri:render-uri
   (quri:make-uri
    :query `(("_src" . ,(guess-src))
             ("_n" . ,(string-downcase (nibble-name nibble))))
    :defaults (format nil "~a~a"
                      (nibble-prefix (nibble-acceptor nibble))
                      (slot-value nibble 'id)))))


(defmethod markup:format-attr-val (stream (nibble nibble))
  (format stream "\"~a\""
          (nibble-url nibble)))

(defmethod hex:safe-redirect ((nibble nibble) &rest args)
  (apply 'hex:safe-redirect (nibble-url nibble)
          args))

(defmacro defnibble (name args &body body)
  `(progn
     (defun ,name (&key ,@args)
       ,@body)
     (setf (assoc-value *named-nibbles* ',name)
           ',args)))


(cl-cron:make-cron-job 'gc :minute 43
                       :hash-key 'gc)

(defmethod print-object ((nibble nibble) out)
  (format out "#<NIBBLE ~a>" (ignore-errors (slot-value nibble 'impl))))
