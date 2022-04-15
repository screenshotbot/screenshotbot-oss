;; Copyright 2019, Modern Interpreters Inc

(defpackage :markup/optimizer
  (:use #:cl)
  (:import-from #:markup/markup
                #:optimize-markup
                #:make-toplevel-node
                #:abstract-xml-tag
                #:xml-tag
                #:xml-tag-attributes
                #:write-attribute-pair
                #:get-markup-fn
                #:standard-name?
                #:write-html-to-stream
                #:xml-tag-children
                #:xml-tag-name
                #:make-xml-tag)
  (:local-nicknames (#:a #:alexandria)))
(in-package :markup/optimizer)

(defclass lazy-xml-tag (abstract-xml-tag)
  ((delegate )
   (xml-tag-builder :initarg :builder
                    :reader xml-tag-builder)
   (standard-names :initarg :standard-names
                   :reader standard-names
                   :documentation "A list of all symbols that are
                  assumed to be standard names. At compilation time,
                  the FAST-WRITER might detect that these symbols are
                  not overriden.. If at render time we detect that one
                  of the symbols are overriden, then we'll fallback to
                  the slow path. This should be very very rare." )
   (fast-writer :initarg :fast-writer
                :reader fast-writer
                :documentation "A lambda function, when called with a
                stream will very efficiently write the tree to the
                stream. This function will only work if you didn't
                attempt to introspect the node.")))

(defmethod print-object :around ((tree lazy-xml-tag) stream)
  (handler-case
      (print-object
       (funcall (xml-tag-builder tree))
       stream)
    (error (e)
      (format stream "#<LAZY-XML-TAG error printing>"))))

(defvar *disable-optimizer* nil)

(defmethod write-html-to-stream ((self lazy-xml-tag) stream)
  (cond
    ((or *disable-optimizer* (slot-boundp self 'delegate))
     (write-html-to-stream (delegate self) stream))
    (t
     (loop for name in (standard-names self)
           if (get-markup-fn name)
             do
                ;; invalidated, because one of the standard names were
                ;; overriden
                (write-html-to-stream (delegate self) stream)
           finally
            (funcall (fast-writer self) stream)))))

(defun delegate (tag)
  (if (slot-boundp tag 'delegate)
      (slot-value tag 'delegate)
      (progn
        (setf (slot-value tag 'delegate)
             (funcall (xml-tag-builder tag))))))

(defmethod xml-tag-name ((tag abstract-xml-tag))
  (xml-tag-name (delegate tag)))

(defmethod xml-tag-attributes ((Tag abstract-xml-tag))
  (xml-tag-attributes (delegate tag)))

(defmethod (setf xml-tag-attributes) (value (tag abstract-xml-tag))
  (setf (xml-tag-attributes (delegate tag)) value))

(defmethod xml-tag-children ((Tag abstract-xml-tag))
  (xml-tag-children (delegate tag)))

(defmethod optimize-markup ((tree t))
  tree)

(defmethod optimize-markup ((tree cons))
  "Rewrite the tree of (make-xml-tag ...)s into something that can be
  more efficiently rendered in the most frequent case."

  ;; First we rewrite any "parameters" into a top-level LET. This
  ;; let's us do more cleverer operations on this later.
  (let ((params)
        (register-counter 0)
        (standard-names '()))
    (labels ((make-sym ()
               (intern (format nil "R~a" (incf register-counter))))
             (walk-alist (x)
               (when x
                 (assert (eql 'list (car x)))
                 (list* 'list
                         (loop for (nil k  v) in (cdr x)
                               collect `(cons ,k ,(walk v))))))
             (walk-list (x)
               (when x ;; for test readability
                 (assert (eql 'list (car x)))
                 (list* 'list
                         (mapcar #'walk (cdr x)))))
             (evaluate-separately (sexp)
               (let ((sym (make-sym)))
                 (push (list sym sexp) params)
                 sym))
             (walk (sexp)
               "Replaces the node with the optimized version, with all
             parameters replaced and rewritten in params."
               (cond
                 ((null sexp)
                  sexp)
                 ((and (consp sexp)
                       (eql 'make-xml-tag (car sexp)))
                  (cond
                    ((or
                      (keywordp (cadr sexp))
                      (let ((name (cadr (cadr sexp))))
                        (and
                         (standard-name? name)
                         (not (get-markup-fn name)))))
                     (destructuring-bind (name &key attributes children unused)
                         (cdr sexp)
                       (unless (keywordp name)
                         (pushnew (cadr name) standard-names))
                       (list
                        'make-xml-tag
                        name
                        :attributes (walk-alist attributes)
                        :children (walk-list children)
                        :unused unused)))
                    (t
                     (destructuring-bind (name &key attributes children unused)
                         (cdr sexp)
                       (evaluate-separately
                        (list
                         'make-xml-tag
                          name
                          :attributes attributes
                          :children (when children
                                      `(list ,@(loop for child in (cdr children)
                                                    collect `(make-toplevel-node ,child))))
                         :unused unused))))))
                 ((and (consp sexp)
                       (stringp (car sexp)))
                  ;; attributes?
                  (cons
                   (car sexp)
                   (walk (cdr sexp))))
                 ((and
                   (atom sexp)
                   (not (symbolp sexp)))
                  sexp)
                 (t
                  ;; this needs to be evaluated separately
                  (evaluate-separately sexp)))))
      (let ((inner (walk tree)))

        ;; At this point `inner` is comprised of just MAKE-XML-TAG,
        ;; LIST, CONS, and symbols referening registers or symbols
        ;; referencing names. This allows for an easier optimization.

        `(let ,(reverse params)
           ,(cond
             ((symbolp inner)
              inner)
             (t
              `(make-lazy-xml-tag
                ,(mapcar #'car (reverse params))
                ,standard-names
                ,inner))))))))

(defclass lambda-builder-stream ()
  ((body :initarg :body
         :initform nil
         :accessor body))
  (:documentation "A fake 'stream'. We'll try to write a fake xml-tree
  into this, but instead of writing the actual contents, we'll write
  the instructions required to reproduce this efficiently."))

(define-condition render-register (condition)
  ((register :initarg :register
             :reader register)))

(define-condition render-register-attr (condition)
  ((register :initarg :register
             :reader register)
   (name :initarg :name
         :reader attr-name)))

(defmethod %write-html-to-stream ((self xml-tag) (stream lambda-builder-stream))
  (let ((out (make-string-output-stream)))
    (flet ((flush ()
             (let ((str (get-output-stream-string out)))
               (unless (str:emptyp str)
                (push `(write-string ,str stream) (body stream))))))
      (handler-bind ((render-register (lambda (condition)
                                        (flush)
                                        (push `(write-html-to-stream ,(register condition)
                                                                     stream)
                                              (body stream))))
                     (render-register-attr
                       (lambda (condition)
                         (flush)
                         (push `(when ,(register condition)
                                  (write-attribute-pair ,(attr-name condition) ,(register condition) stream))
                               (body stream)))))
        (write-html-to-stream self out))
      (flush))))

(defclass register-tag ()
  ((register :initarg :register
             :reader register)))

(defmethod write-html-to-stream ((self register-tag) stream)
  (signal 'render-register :register (register self)))

(defmethod write-attribute-pair (name (self register-tag) stream)
  (signal 'render-register-attr :register (register self)
           :name name))

(defmacro make-lazy-xml-tag (registers standard-names body)
  ;;(error "got registers: ~s" registers)
  `(make-instance 'lazy-xml-tag
                   :builder (lambda ()
                              ,body)
                   :standard-names ',standard-names
                   :fast-writer (alexandria:named-lambda fast-writer (stream)
                                  (declare (optimize (speed 3)))
                                  ,@(let ((stream (make-instance 'lambda-builder-stream)))

                                      (%write-html-to-stream
                                       (eval
                                        `(symbol-macrolet
                                             ,(loop for register in registers
                                                    collect
                                                    `(,register (make-instance 'register-tag
                                                                                :register ',register)))
                                           ,body))
                                       stream)
                                      (nreverse  (body stream))))))
