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
   (acceptor-plugin :initarg :acceptor-plugin
                    :initform nil
                    :reader nibble-acceptor-plugin
                    :documentation "The plugin where this nibble was created from")
   (session :initarg :session)
   (user :initarg :user
         :initform nil
         :accessor nibble-user)
   (check-session-p :initarg :check-session-p
                    :initform nil)
   (ts :initarg :ts)))

(defmethod print-object ((nibble nibble) out)
  (format out "#<NIBBLE ~a>" (ignore-errors (slot-value nibble 'impl))))

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

(defclass nibble-plugin (hex:acceptor-plugin)
  ((wrapper :initarg :wrapper
            :initform 'funcall
            :accessor nibble-plugin-wrapper))
  (:default-initargs :prefix "/n/"))

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
         (nibble (make-instance 'nibble
                                 :impl impl
                                 :name name
                                 :session (current-session)
                                 :acceptor-plugin (when (boundp 'hex:*acceptor-plugin*)
                                                    hex:*acceptor-plugin*)
                                 :user
                                 (cond
                                   ((boundp 'hunchentoot:*acceptor*)
                                    (nibble-current-user hunchentoot:*acceptor*))
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

(hex:define-plugin-handler (run-nibble :uri "/:id" :plugin-name 'nibble-plugin) (id)
  (render-nibble hex:*acceptor-plugin* id))

(defmethod render-nibble ((plugin nibble-plugin) (nibble nibble))
  (render-nibble plugin (nibble-id nibble)))

(defmethod nibble-current-user (acceptor)
  "Override this to ensure that if the user logs out, we will not
  allow them to see the nibble page. In order for the page to be
  available, both the session and the logged in user should be the
  same when the nibble was created and when the nibble is viewed

  The objects returned by this are compared with EQL"
  nil)

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
         :initform nil))
  (:report (lambda (self stream)
             (with-slots (name) self
               (format stream "Expired nibble with name ~a was accessed" name)))))

(defmethod render-nibble ((plugin nibble-plugin) id)
  (funcall
   (nibble-plugin-wrapper plugin)
   (lambda ()
     (let ((id (cond
                 ((stringp id) (parse-integer id))
                 (t id))))
       (let ((nibble (get-nibble id)))
         (cond
           ((null nibble)
            (warn 'expired-nibble :name (safe-parameter :_n))
            (setf (hunchentoot:return-code*) 410 #| GONE |# )
            (setf (hunchentoot:header-out :x-expired-nibble) "1")
            <html>
              <body>
                The page you're looking for has expired.
                <a href= "/">Go back</a>
              </body>
            </html>)
           (t
            (with-slots (impl args session once check-session-p) nibble
              (let ((args (loop for arg in args
                                collect (safe-parameter arg))))
                (flet ((final-render ()
                         (when once
                           ;; we need to make sure only one call of this nibble
                           ;; happens.
                           (bt:with-lock-held (*lock*)
                             (when (calledp nibble)
                               (error "Calling nibble multiple times"))
                             (setf (calledp nibble) t)))
                         (let ((hex:*acceptor-plugin* (nibble-acceptor-plugin nibble)))
                          (apply impl args))))
                 (cond
                   ((and (boundp 'hunchentoot:*request*) check-session-p)
                    ;; Before we call final-render, we should check
                    ;; the session and logged in user.
                    (let ((current-session (current-session)))
                      (cond
                        ((not (auth:session= session current-session))
                         (render-incorrect-session plugin))
                        ((let ((nibble-user (nibble-user nibble)))
                           (and
                            ;; if the nibble was created when they
                            ;; weren't logged in, and they are logged
                            ;; in now, that's always okay
                            nibble-user
                            (not (eql (nibble-current-user hunchentoot:*acceptor*)
                                      nibble-user))))
                         (nibble-render-logged-out
                          hunchentoot:*acceptor*
                          nibble))
                        (t
                         (final-render)))))
                   (t
                    (final-render)))))))))))))

(defmethod render-incorrect-session ((plugin nibble-plugin))
  <html>
    <body>
      <h1>You cannot view this page.</h1>
      <p>This URL is tied to a specific browser and user. You should not share this URL, or copy-and-paste
        it into another browser.</p>
      <a href= "/">Go back</a>
    </body>
  </html>)

(defun nibble-full-url (nibble)
  (apply #'hex:make-full-url
   hunchentoot:*request*
   'run-nibble
   :id (slot-value nibble 'id)
   (when (nibble-name nibble)
     `(:_n ,(string-downcase (nibble-name nibble))))))

(defun nibble-url (nibble)
  (apply #'hex:make-url
           'run-nibble
            :id (slot-value nibble 'id)
            (when (nibble-name nibble)
              `(:_n ,(string-downcase (nibble-name nibble))))))


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
