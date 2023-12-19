;; Copyright 2018-Present Modern Interpreters Inc.
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/testing
  (:use #:cl)
  (:import-from #:nibble
                #:nibble-plugin)
  (:import-from #:hunchentoot
                #:acceptor)
  (:import-from #:easy-macros
                #:def-easy-macro)
  (:import-from #:util/misc
                #:with-global-binding)
  (:import-from #:alexandria
                #:assoc-value)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:with-fake-request
   #:in-test-p
   #:screenshot-static-page
   #:with-local-acceptor
   #:with-global-binding
   #:define-screenshot-test-init
   #:clean-up-static-web-outputs))
(in-package :util/testing)

(defvar *in-test-p* nil)

(defun in-test-p ()
  *in-test-p*)

(defclass custom-request (auth:authenticated-request)
  ((additional-post-params :initform nil)))

(defmethod (setf hunchentoot:post-parameter) (val (request custom-request) key)
  (with-slots (additional-post-params) request
    (setf (a:assoc-value additional-post-params key :test 'equal)
          val)))

(defmethod hunchentoot:post-parameters ((request custom-request))
  (with-slots (additional-post-params) request
   (append
    additional-post-params
    (call-next-method))))

(defclass test-acceptor (hex:base-acceptor
                         hex:acceptor-with-plugins)
  ()
  (:default-initargs :acceptor-plugins (list (make-instance 'nibble-plugin))))

(defmacro with-fake-request ((&key  (acceptor '(quote test-acceptor)) (host "localhost")
                                (script-name "/")
                                (params)) &body body)
 `(let* ((hunchentoot::*hunchentoot-stream*)
         (hunchentoot:*catch-errors-p* nil)
         (hunchentoot:*acceptor* (make-instance ,acceptor))
         (hunchentoot:*reply* (make-instance 'hunchentoot:reply))
         (hunchentoot:*request* (make-instance 'custom-request
                                               :acceptor hunchentoot:*acceptor*
                                               :headers-in (list (cons :host ,host))
                                               :content-stream nil
                                               :uri ,script-name
                                               :method :get
                                               :server-protocol :https
                                               :remote-addr "127.0.0.1")))
    ,@body))


(defvar *prepared* (make-hash-table))

(defun maybe-prepare-screenshot-assets (name dir
                                        &key static-assets
                                          generated-css-assets)
  (or
   (gethash name *prepared*)
   (setf
    (gethash name *prepared*)
    (prog1
        t
      (copy-directory:copy
       static-assets
       (path:catdir dir "assets/"))
      (flet ((copy-css (target output)
               (asdf:compile-system target)
               (uiop:copy-file
                (car (asdf:output-files 'asdf:compile-op target))
                (path:catfile dir output))))
        (loop for (key . val) in generated-css-assets do
          (copy-css key val)))))))

(defvar *screenshot-test-inits* nil)

(defmacro define-screenshot-test-init (project &key static-assets generated-css-assets)
  `(let ((static-assets (asdf:system-relative-pathname ',project ,static-assets)))
       (setf (assoc-value *screenshot-test-inits* ',project :test #'string-equal)
             (lambda ()
               (maybe-prepare-screenshot-assets
                ',project
                (static-web-output-dir ',project)
                :static-assets static-assets
                :generated-css-assets ,generated-css-assets)))))


(let ((cache (make-hash-table :test #'equal)))
  (defun static-web-output-dir (project)
    (util:or-setf
     (gethash project cache)
     (asdf:system-relative-pathname project "static-web-output/"))))

(defun screenshot-static-page (project name content)
  (let ((output (static-web-output-dir project)))
    (let ((output-file (path:catfile output (format nil "~a/index.html" name))))
      (ensure-directories-exist output-file)
      (funcall
       (assoc-value *screenshot-test-inits* project :test #'string-equal))
      (with-open-file (file output-file
                            :direction :output
                            :external-format :utf-8
                            :if-exists :supersede)
        (let ((content (typecase content
                         (string content)
                         (t
                          (cl-mock:with-mocks ()
                            (cl-mock:if-called 'nibble:nibble-url (lambda (nibble)
                                                                    "#"))
                            (markup:write-html content))))))
          (write-string content file))
        (fiveam:pass "Screenshot written")))))

(defun clean-up-static-web-outputs ()
  (dolist (project (mapcar #'car *screenshot-test-inits*)) do
    (let ((dir (asdf:system-relative-pathname project "static-web-output/")))
      (when (path:-d dir)
        (uiop:delete-directory-tree
         dir
         :validate (lambda (x)
                     (declare (ignore x))
                     t))))))



(defmacro with-local-acceptor ((host &key prepare-acceptor-callback (acceptor (gensym))) (name &rest args) &body body)
  "Create a debuggable single threaded acceptor for running tests"
  `(flet ((fn (,host ,acceptor)
            (declare (ignorable ,acceptor))
            ,@body))
     (call-with-local-acceptor #'fn ,prepare-acceptor-callback ,name (list ,@args))))

(defmethod safe-start ((acceptor acceptor) &key listen-callback)
  ;; this is copied from hunchentoot:start except for listen-callback
  (setf (hunchentoot::acceptor-shutdown-p acceptor) nil)
  (let ((taskmaster (hunchentoot::acceptor-taskmaster acceptor)))
    (setf (hunchentoot:taskmaster-acceptor taskmaster) acceptor)
    (hunchentoot:start-listening acceptor)
    (funcall listen-callback)
    (hunchentoot:execute-acceptor taskmaster))
  acceptor)

(defclass debuggable-taskmaster (hunchentoot:single-threaded-taskmaster)
  ())

(defun call-with-local-acceptor (fn prepare-acceptor-callback name args)
  (let ((port (util/random-port:random-port)))
    (let ((acceptor (apply #'make-instance name
                           :message-log-destination *standard-output*
                           :port port
                           :taskmaster (make-instance 'debuggable-taskmaster)
                           args)))
     (when prepare-acceptor-callback
       (funcall prepare-acceptor-callback acceptor))
     (let ((lock (bt:make-lock))
           (acceptor-ready (bt:make-condition-variable))
           (thread-crashed? nil)
           (catch-errors? hunchentoot:*catch-errors-p*))
       (bt:with-lock-held (lock)
         (let ((thread (bt:make-thread
                        (lambda ()
                         (restart-case
                             (handler-bind ((error (lambda (e)
                                                     (when catch-errors? ;; in non interactive mode
                                                       (trivial-backtrace:print-backtrace e)
                                                       (invoke-restart 'exit-acceptor)))))
                               (let ((hunchentoot:*catch-errors-p* nil))
                                 (safe-start
                                  acceptor
                                  :listen-callback
                                  (lambda ()
                                    (bt:with-lock-held (lock)
                                      (bt:condition-notify acceptor-ready))))))
                           (exit-acceptor ()
                             nil))))))
           (bt:condition-wait acceptor-ready lock)
           (unwind-protect
                (funcall fn (format nil "http://127.0.0.1:~d" port)
                         acceptor)
             (hunchentoot:stop acceptor)
             (bt:join-thread thread)
             (when thread-crashed?
               (error "The acceptor thread crashed, see logs")))))))))
