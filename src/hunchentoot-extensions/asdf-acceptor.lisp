(defpackage :hunchentoot-extensions/asdf-acceptor
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:asdf-acceptor
   #:define-asdf-handler))
(in-package :hunchentoot-extensions/asdf-acceptor)

(defclass asdf-asset ()
  ((system :initarg :system
           :reader system)
   (url :initarg :url)
   (content-type :initarg :content-type
                :reader content-type)))

(defclass asdf-acceptor ()
  ((assets :initform (make-hash-table :test #'equal)
           :reader assets))
  (:documentation "An acceptor that responds to requests from ASDF (whether pre-compiled or dynamic)"))

(defmethod define-asdf-handler ((self asdf-acceptor)
                                &rest args
                                &key (url (error "must provide :url"))
                                  (system (error "must provide :system"))
                                  content-type)
  (declare (ignore system content-type))
  (setf (gethash url (assets self))
        (apply #'make-instance 'asdf-asset
               args)))

(defmethod handle-asdf-output ((self asdf-asset))
  (asdf:operate 'asdf:compile-op (system self))
  (setf (hunchentoot:content-type*) (content-type self))
  (hunchentoot:handle-static-file
   (car (asdf:output-files 'asdf:compile-op (system self)))))

(defmethod hunchentoot:acceptor-dispatch-request ((self asdf-acceptor) request)
  (let* ((script-name (hunchentoot:script-name request))
         (asset (gethash script-name (assets self))))
    (cond
      (asset
       (handle-asdf-output asset))
      (t
       (call-next-method)))))
