(in-package :util)

(defclass custom-request (hunchentoot:request)
  ((additional-post-params :initform nil)))

(defmethod (setf hunchentoot:post-parameter) (val (request custom-request) key)
  (with-slots (additional-post-params) request
    (setf (assoc-value additional-post-params key :test 'equal)
          val)))

(defmethod hunchentoot:post-parameters ((request custom-request))
  (with-slots (additional-post-params) request
   (append
    additional-post-params
    (call-next-method))))

(defmacro with-fake-request ((&key  (acceptor '(quote util:base-acceptor)) (host "localhost")
                                (script-name "/")) &body body)
 `(let* ((hunchentoot::*hunchentoot-stream*)
         (hunchentoot:*catch-errors-p* nil)
         (hunchentoot:*acceptor* (make-instance ,acceptor
                                                :db-config '(:mysqladmin nil)))
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
