(defpackage :remark/markdown
  (:use #:cl
        #:iterate)
  (:shadow #:merge)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:md
   #:header-id))
(in-package :remark/markdown)

(markup:enable-reader)

;; TODO: \\W is incorrect here

(defparameter *scanner* (cl-ppcre:create-scanner "\\n(\\W*\\n)+" :multi-line-mode t))


(defun header-tag-for (&key length)
  "By keeping header-tag-for as a separate function, we can
programmatically change the depth at which a specific node might be."
  (ecase length
    (1 'h1)
    (2 'h2)
    (3 'h3)))

(defun header-id (content)
  (string-downcase
   (str:replace-all "'" ""
    (str:replace-all " " "-"
                     (format nil "~a" content)))))

(defun format-para (para)
  (let* ((nodes (paragraph-nodes para))
         (first-node (car nodes)))
    (cond
      ((and
        (stringp first-node)
        (str:starts-with-p "#" (str:trim-left first-node)))
       (let* ((parts (str:split " " (str:trim-left first-node) :limit 2))
              (h (car parts))
              (content (cadr parts)))
         (let ((h-type (cond
                         ((or
                           (equal "#" h)
                           (equal "##" h)
                           (equal "###" h))
                          (header-tag-for :length (length h)))
                         (t
                          (error "Invalid header directive: `~a`" h)))))
           (markup:make-xml-tag h-type
                                :attributes (list
                                             (cons "id" (header-id content)))
                                :children (list*
                                           (str:trim content)
                                           (cdr nodes))))))
      ((stringp first-node)
       <p>,(progn (str:trim-left first-node)),@(cdr nodes)</p>)
      (t
       <p>,@(progn nodes)</p>))))


(defclass paragraph ()
  ((nodes :initarg :nodes
          :initform nil
          :accessor %paragraph-nodes)))


(defun paragraph-nodes (paragraph)
  (reverse (%paragraph-nodes paragraph)))

(defun push-to-para (node paragraph)
  (push node (%paragraph-nodes paragraph)))

(defun paragraphp (x)
  (typep x 'paragraph))

(defun merge-adjacent-strings (list)
  (declare (optimize (debug 3)) )
  (labels ((merge (current-para list ret)
             (declare (optimize (Debug 3)))
             (cond
               ((not list)
                (nreverse (list* current-para ret)))
               (t
                (destructuring-bind (next . rest) list
                  (cond
                    ((stringp next)
                     (destructuring-bind (open &optional remaining-para)
                         (cl-ppcre:split *scanner* next :limit 2)
                       (let ((current-para (make-instance 'paragraph
                                                           :nodes (list*
                                                                   open
                                                                   (%paragraph-nodes current-para)))))
                         (cond
                           (remaining-para
                            (merge
                             (make-instance 'paragraph)
                             (list* remaining-para  rest)
                             (list* current-para ret)))
                           (t
                            (merge
                             current-para
                             rest
                             ret))))
                       ))
                    ((typep next 'markup:abstract-xml-tag)
                     (cond
                       ((member (markup:xml-tag-name next) '(:b :em :a :tt :span))
                        (merge
                         (make-instance 'paragraph
                                         :nodes (list*
                                                 next
                                                 (%paragraph-nodes current-para)))
                         rest
                         ret))
                       (t
                        (merge
                         (make-instance 'paragraph)
                         rest
                         (list* (make-instance 'paragraph :nodes (list next))
                                ret)))))
                    (t
                     (error "unknown type: ~a" (type-of next)))))))))
    (merge
     (make-instance 'paragraph)
     list
     nil)))

(markup:deftag md (children)
  (markup:make-merge-tag
   (mapcar #'format-para (merge-adjacent-strings children))))
