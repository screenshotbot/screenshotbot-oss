;; Copyright 2018-Present Modern Interpreters Inc.
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(in-package :hex)

(defun add-get-param-to-url (url name value)
  "URL is assumed to be a http URL. The pair of NAME and VALUE will be
added as a GET parameter to this URL. Assumes that there's no other
parameter of the same name. Only checks if #\? is part of the string
to decide how to attach the new parameter to the end of the string."
  ;; possible bug: doesn't check for #\? which is written as, say,
  ;; "&x3f;" - also, is there any other way a question mark could be a
  ;; legitimate part of a URL?
  (concatenate 'string
               url
               (if (find #\? url :test #'char=)
                 "&"
                 "?")
               name
               "="
               (hunchentoot:url-encode value)))

(defun %required-args (path)
  (ecase (car path)
    (:path
     nil)
    (:root
     nil)
    (:variable
     (let ((name (intern (string-upcase (cadr path)) "KEYWORD")))
       (list name)))
    (:join
     (append (%required-args (cadr path))
             (%required-args (caddr path))))
    (:optional nil)))

(define-condition missing-required-arg ()
  ((arg :initarg :arg)))

(defun %apply-args-to-parse-tree (path args)
  (assert path)
  (ecase (car path)
    (:root
     "/")
    (:path
     (values (format nil "/~a" (cadr path))args))
    (:variable
     (let ((name (intern (string-upcase (cadr path)) "KEYWORD")))
       (let ((value (assoc-value args name)))
         (unless value
           (error 'missing-required-arg :arg name))
         (values (format nil "/~a" (urlencode:urlencode (format nil "~a"  value)))
                 (remove name args :key 'car)))))
    (:optional
     ;; only render this if all required args are present
     (let ((req (%required-args (cadr path))))
       (log:info "required args: ~s" req)
       (if (loop for r in req always
                              (assoc-value args r))
           (%apply-args-to-parse-tree (cadr path) args)
           "")))
    (:join
     (multiple-value-bind (path1 args) (%apply-args-to-parse-tree (cadr path) args)
       (multiple-value-bind (path2 args)
           (%apply-args-to-parse-tree (caddr path) args)
         (values (format nil "~a~a" path1 path2) args))))))

(defun make-full-url (request &rest make-url-args)
  (let ((part (apply 'make-url make-url-args)))
    (format nil "~a~a"
            (get-request-domain-prefix request)
            part)))

(defun make-url (path &rest rest &key &allow-other-keys)
  (cond
    ((and
      (stringp path)
      (or
       (str:starts-with-p "http:" path)
       (str:starts-with-p "https:" path)))
     ;; note that this path of addint params is very different from
     ;; the when we're dealint giwht url handlers since, in that case
     ;; you might have things like /foo/:bar.
     (let ((url (quri:uri path)))
       (loop for (key value)  on rest by #'cddr do
         (push (cons (string-downcase key) value) (quri:uri-query-params url)))
       (quri:render-uri url)))
    ((string= "/" path)
     "/")
    (t
     (let* ((url-handler (when (symbolp path)
                           (let ((url-handler(alexandria:assoc-value *url-list* path)))
                             (unless url-handler
                               (error "No url handler for the name: ~s" path))
                               url-handler)))
            (parse-tree (if url-handler
                            (url-handler-parse-tree url-handler)
                            (%make-uri-parse-tree path))))
       (let ((args (alexandria:plist-alist rest)))
         (multiple-value-bind (path args) (%apply-args-to-parse-tree parse-tree args)

           (let ((ret (format nil "~a~a" (if url-handler
                                             (url-handler-prefix url-handler)
                                             "")
                              path)))
             (flet ((add (key value) (setf ret (add-get-param-to-url ret (string-downcase key)
                                                                     (format nil "~a" value)))))
               (loop for (key . value) in args do
                    (cond
                      ((hash-table-p value)
                       (loop for (inner-key . inner-value) in (alexandria:hash-table-alist value) do
                            (add (format nil "~a{~a}" key inner-key) inner-value)))
                      (t (add key value)))))
             ret)))))))

(define-compiler-macro make-url (&whole whole name &rest args)
    (cond
      ((and (listp name)
            (eql 'quote (car name)))
       (let ((name (cadr name)))
         (unless (get name 'handlerp)
           #-ccl ;; the tests fail on CCL for some reason
           (error "make-url called with symbol ~S, but it has not been created as a handler yet"
                  name)))
       whole)
      (t whole)))

(defun get-request-domain-prefix (&optional (request hunchentoot:*request*))
  "Like https://xyz.com or http://localhost:3014. Appropriate for use
  in emails"
  (destructuring-bind (host &optional (port "80")) (str:split ":" (hunchentoot:host request))
    (let ((port (parse-integer port)))
     (case port
       (443 (format nil "https://~a" host))
       (80 (format nil "~a://~a"
                   (or
                    (hunchentoot:header-in :x-forwarded-proto request)
                    (hunchentoot:header-in :x-forwarded-proto request)
                    "http")
                   host))
       (otherwise (format nil "http://~a:~a" host port))))))
