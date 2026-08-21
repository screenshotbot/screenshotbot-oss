;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(pkg:define-package :util/phabricator/conduit
  (:use #:cl
        #:alexandria)
  (:import-from #:util/request
                #:http-request)
  (:import-from #:alexandria
                #:alist-hash-table)
  (:export #:phab-instance
           #:call-conduit
           #:search-conduit
           #:url
           #:api-key
           #:make-phab-instance-from-arcrc
           #:whoami))

(defclass phab-instance ()
  ((url :initarg :url
        :accessor url)
   (api-key :initarg :api-key
            :accessor api-key)))

(defun make-phab-instance-from-arcrc (url)
  (let* ((api-url (quri:render-uri (quri:merge-uris
                                    (quri:uri "/api/")
                                    (quri:uri url)))))
   (with-open-file (file "~/.arcrc")
     (let* ((json:*json-identifier-name-to-lisp* #'string)
            (arcrc (json:decode-json file))
            (hosts (assoc-value arcrc :|hosts|))
            (host
              ;; Surely there's a better way to do this.
              (assoc-value hosts (intern api-url "KEYWORD")))
            (token
              (assoc-value host :|token|)))
       (assert token)
       (make-instance 'phab-instance
                       :url url
                       :api-key token)))))

(defun phab-test ()
  ;; useful for testing things
  (make-phab-instance-from-arcrc "https://phabricator.tdrhq.com"))


(defmethod call-conduit ((phab phab-instance) name params)
  #+nil(log:debug "initial params: ~s" params)
  (let* ((params (alist-hash-table
                  `(,@params
                    ("__conduit__"
                     .
                     ,(alist-hash-table
                       `(("token" . ,(api-key phab)))))))))
    (log:debug "using params: ~S" params)
    (let* ((encoded (json:encode-json-to-string params))
           (res
             (progn
               (log:info "Running ~a" encoded)
               (http-request
                (format nil "~a/api/~a" (url phab) name)
                :method :post
                :want-string t
                :form-data t
                :parameters `(("params" . ,encoded)
                              ("output" . "json")
                             ("__conduit__" . "1"))))))
     (let* ((res
              (json:decode-json-from-string res))
            (error-info
              (assoc-value res :error--info)))
       (when error-info
         (error "Got conduit error: ~A " (str:shorten 500 error-info)))
       res))))


;; * The *.search methods

(defmethod search-page ((phab phab-instance) method constraints after limit)
  "One page of a *.search method. Returns the rows, and the cursor to
continue from -- NIL when that was the last page."
  (let* ((params `(("limit" . ,(min limit 100))
                   ,@(when after
                       (list (cons "after" after)))
                   ,@(when constraints
                       (list (cons "constraints"
                                   (alist-hash-table constraints :test #'equal))))))
         (result (assoc-value (call-conduit phab method params) :result)))
    (values (assoc-value result :data)
            (assoc-value (assoc-value result :cursor) :after))))

(defmethod search-conduit ((phab phab-instance) method &key constraints (limit 100))
  "The rows a *.search method returns, at most LIMIT of them.

Every *.search method in Conduit answers in pages of at most 100 and
hands back a cursor, so this follows the cursor until it has LIMIT rows
or Phabricator runs out. CONSTRAINTS is an alist, and is passed through
as the method's constraints object.

Returns the rows, and as a second value whether there were more to be
had -- which is a thing a caller that stops early ought to admit to."
  (let ((rows '())
        (after nil)
        (morep nil))
    (loop
      (multiple-value-bind (page next)
          (search-page phab method constraints after (- limit (length rows)))
        (setf rows (append rows page)
              after next)
        (when (or (null page) (null next) (>= (length rows) limit))
          (setf morep (and next (>= (length rows) limit) t))
          (return))))
    (values (if (> (length rows) limit)
                (subseq rows 0 limit)
                rows)
            morep)))

(defmethod whoami ((phab phab-instance))
  (let ((body
         (call-conduit
          phab
          "user.whoami"
          nil)))
    (assoc-value (assoc-value body :result) :phid)))

;; (whoami (phab-test))
