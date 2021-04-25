;; Copyright 2018-Present Modern Interpreters Inc.
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(in-package :hunchentoot-extensions)

;; each element is: (list handler-name plugin-name matcher handler)
(defvar *acceptor-plugins-table* nil)

(defclass acceptor-plugin ()
  ((prefix :initarg :prefix
           :accessor acceptor-plugin-prefix)
   (regex :accessor acceptor-plugin-regex)))

(defclass plugin-url-handler (hex::url-handler)
  ((plugin-name :initarg :plugin-name)))

(defmethod hex::url-handler-prefix ((handler plugin-url-handler))
  (with-slots (plugin-name) handler
   (loop for plugin in (acceptor-plugins hunchentoot:*acceptor*)
         if (eq (acceptor-plugin-name plugin) plugin-name)
           do
              (return
                (let ((prefix (acceptor-plugin-prefix plugin)))
                 (str:substring 0 (1- (length prefix))
                                prefix)))
         finally
         (error "no plugin by name: ~a" plugin-name))))

(defun make-prefix-matcher (prefix)
  (assert (str:ends-with-p "/" prefix))
  (cl-ppcre:create-scanner
   `(:sequence :start-anchor ,(str:substring 0 (- (length prefix) 1) prefix)
              (:regex "(/.*)?$"))))

(defmethod initialize-instance :after ((plugin acceptor-plugin) &key &allow-other-keys)
  (with-slots (prefix) plugin
   (setf (acceptor-plugin-regex plugin)
         (make-prefix-matcher prefix))))

(defmethod acceptor-plugin-name (plugin)
  (type-of plugin))

(defvar *acceptor-plugin*)

(defclass acceptor-with-plugins (hunchentoot:easy-acceptor)
  ((acceptor-plugins :accessor acceptor-plugins
                     :initform nil)))

(defmacro define-plugin-handler ((name &key uri method plugin-name) params &body body)
  (declare (ignore method))
  (unless uri
    (setf uri "/nil"))
  (multiple-value-bind (full-regex full-var-list)
      (hex:make-uri-regex uri)
    (declare (ignore full-regex))
   (let* ((param-names (loop for param in params
                             collect (if (listp param) (car param) param)))
          (direct-params (loop for param in param-names
                               if (not (member (string param) full-var-list :test 'string=))
                                 collect param)))
     `(multiple-value-bind (full-regex vars parse-tree) (hex:make-uri-regex ,uri)
        (declare (ignore vars))
        (defun ,name (&key ,@param-names)
          ,@body)
        (setf
         (assoc-value *acceptor-plugins-table* ',name)
         (list ,plugin-name
               full-regex
               (lambda (relative-script-name)
                 (multiple-value-bind (res args)
                     (hex::matches-regex full-regex nil
                                                      :script-name relative-script-name)
                   (declare (ignore res))
                   (,name
                    ,@ (loop for param in direct-params
                             appending
                             (list (intern (string param) (symbol-package :foo))
                                   `(hunchentoot:parameter ,(string param))))
                    ,@ (loop for param in full-var-list
                             for i from 0 to 1000
                             appending
                             (list (intern (string param) (symbol-package :foo))
                                   `(elt args ,i))))))))
        (setf (alexandria:assoc-value hex::*url-list* ',name)
              (make-instance 'plugin-url-handler
                             :parse-tree parse-tree
                             :plugin-name ,plugin-name
                             :request-args ',param-names))))))


(defmethod dispatch-plugin-request ((plugin acceptor-plugin)
                                    request
                                    relative-script-name)
  (loop for (handler-name plugin-name matcher handler) in *acceptor-plugins-table*
        if (and (eq plugin-name (acceptor-plugin-name plugin))
                (cl-ppcre:scan matcher relative-script-name))
          do
             (progn
               (return
                 (let ((*acceptor-plugin* plugin))
                  (funcall handler relative-script-name))))
        finally
           (error "could not dispatch, technically a 404")))

(defmethod wrap-template (acceptor plugin output)
  output)

(defmethod hunchentoot:acceptor-dispatch-request ((acceptor acceptor-with-plugins)
                                                  request)
  (loop for plugin in (acceptor-plugins acceptor)
        if (cl-ppcre:scan (acceptor-plugin-regex plugin) (hunchentoot:script-name request))
          do
             (return
               (markup:write-html
                (wrap-template acceptor plugin
                               (dispatch-plugin-request
                                plugin
                                request
                                (let ((url (str:substring (- (length (acceptor-plugin-prefix plugin)) 1) nil
                                                          (hunchentoot:script-name request))))
                                  (cond
                                    ((eq 0 (length url))
                                     "/")
                                    (t url)))))))
        finally (return (call-next-method))))
