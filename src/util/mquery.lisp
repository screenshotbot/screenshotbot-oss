;; Copyright 2018-Present Modern Interpreters Inc.
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(in-package :mquery)

(defvar *document*)

(defun split-query-components (query)
  ;; technically this regex won't account for
  (flet ((invalid-query () (error "invalid query: ~s" query))
         (make-response (parts)
           `(,(elt parts 0)
              ,(elt parts 1))))
    (cond
     ((eql #\[ (elt query 0))
      ;; if the first letter is [, then we find the closing
      ;; tag. Ideally, we should also try to read into the string for
      ;; escaped ], but we'll skip that for now.
      (multiple-value-bind (res parts) (cl-ppcre:scan-to-strings "^([^\\]]*\\])(.*)?$" query)
        (unless res
          (invalid-query))
        (make-response parts)))
     (t
      (multiple-value-bind (res parts) (cl-ppcre:scan-to-strings "^([.]?[^.\\[]+)([.\\[](.*))?$" query)
        (unless res
          (invalid-query))
        (make-response parts))))))

(defun build-matcher (query)
  "Returns a lambda function that matches a given expression"
  (cond
    ((functionp query)
     query)
    ((str:emptyp query)
     (lambda (x)
       (declare (ignore x))
       t))
    (t
     (destructuring-bind (next rest) (split-query-components query)
       (let ((rest-matcher (build-matcher rest))
             (next-matcher (cond
                             ((str:starts-with-p "[" next)
                              (multiple-value-bind (res parts) (cl-ppcre:scan-to-strings "^\\[(.*)='(.*)'\\]$" next)
                                (unless res
                                  (error "invalid query: ~s" next))
                                (let ((parts (loop for p across parts collect p)))
                                  (destructuring-bind (name val) parts
                                   (lambda (x)
                                     (equal val (mquery:attr x name)))))))
                             ((str:starts-with-p "." next)
                              (let ((class-name (str:substring 1 nil next)))
                               (lambda (x)
                                 (member class-name (css-classes x) :test 'equal))))
                             (t
                              (let ((upcase (string-upcase next)))
                               (lambda (x)
                                 (equal upcase
                                        (string-upcase (xml-tag-name x)))))))))
         (lambda (x)
          (and
           (funcall next-matcher x)
           (funcall rest-matcher x))))))))

(defun namequery (name)
  (let ((name (string-downcase name)))
   (lambda (x)
     (equal name (attr x "name")))))

(defun $ (query &optional (root *document*))
  (cond
    ((typep query 'xml-tag)
     query)
    (t
     (let ((query (build-matcher query)))
      (let ((root (listify root)))
        (let ((ret nil))
          (loop for root in root do
               (markup:walk
                root
                (lambda (x)
                  (assert x)
                  (when (funcall query x)
                    (push x ret))
                  x)))
          (nreverse ret)))))))

(defun parent (x &optional (root *document*))
  (let ((x (only x)))
   (markup:walk
    root
    (lambda (inner)
      (when (member x (xml-tag-children inner))
        (return-from parent inner))
      inner))))

(defun findc (elm list &key (test 'eql))
  (declare (optimize (speed 3)))
  (cond
    ((not list) nil)
    ((funcall test elm (car list)) list)
    (t (findc elm (cdr list) :test test))))

(macrolet ((with-vars (&body body)
             `(let* ((x (only x))
                     (parent (parent x))
                     (children (xml-tag-children parent))
                     (cell (findc x children)))
                ,@body)))
 (defun after (x)
   (with-vars
       (loop for x in (cdr cell) do
            (when (typep x 'xml-tag)
              (return-from after x)))))
 (defun (setf after) (val x)
   (with-vars
       (assert parent)
       (assert children)
       (assert cell)
     (setf (cdr cell)
           (cons val (cdr cell))))))


(defmacro with-document ((doc) &body body)
  `(let ((*document* ,doc))
     ,@body))

(defun only (x)
  (if (listp x)
      (first x)
      x))

(defun listify (root)
  (if (listp root) root (list root)))

(defun attr (x name)
  (let ((x (only x))
        (name (string-downcase name)))
    (when x
     (assoc-value (markup:xml-tag-attributes x) name :test 'equal))))

(defun (setf attr) (val x name)
  (let ((name (string-downcase name)))
   (loop for x in (listify x) do
        (setf (assoc-value (markup:xml-tag-attributes x) name :test 'equal)
              val)))
  val)

(defun css-classes (x)
  (let ((classes (str:split " " (or (attr x "class") ""))))
    classes))

(defun has-class-p (x class-name)
  (member class-name (css-classes x)
          :test 'string=))

(defun (setf css-classes) (classes x)
  (setf (attr x "class") (str:join " " classes))
  classes)

(defun add-class (x class-name)
  (dolist (x (listify x))
   (let ((classes (nreverse (css-classes x))))
     (pushnew class-name classes :test 'equal)
     (setf (css-classes x) (nreverse classes)))))

(defun remove-class (x class-name)
  (dolist (x (listify x))
   (let ((classes (css-classes x)))
     (setf (css-classes x) (delete class-name classes :test 'equal)))))

(defun val-is-attr-p (tag)
  (member (xml-tag-name tag)
          '(:input :button)))

(defun val-is-inner-html (tag)
  (member (xml-tag-name tag)
          '(:textarea)))

(defun val-is-checkbox (tag)
  (and (eql :input (xml-tag-name tag))
       (string= "checkbox" (string-downcase (attr tag "type")))))

(defun val (x)
  (let ((x (only x)))
    (cond
      ((val-is-checkbox x)
       (not (str:empty? (attr x "checked"))))
      ((val-is-attr-p x)
       (attr "value"))
      ((val-is-inner-html x)
       (text x))
      (t
       (error "unsupported val type: ~s" x)))))


(defun (setf val) (value x)
  (let ((x (only x)))
    (cond
      ((null x)
       nil)
      ((val-is-checkbox x)
       (setf (attr x "checked")
             (if value "checked" nil)))
      ((val-is-attr-p x)
       (setf (attr x "value") value))
      ((val-is-inner-html x)
       (setf (text x) value))
      (t
       (error "unsupport (Setf val) type: ~s" x)))))

(defun text (x)
  (let ((stream (make-string-output-stream)))
   (let ((x (only x)))
     (labels ((write-stream (x)
                (typecase x
                  (markup:xml-tag
                   (mapc #'write-stream (xml-tag-children x)))
                  (t
                   (format stream "~a" x)))))
       (write-stream x)))
   (get-output-stream-string stream)))

(defun (Setf text) (value x)
  (let ((x (only x)))
    (setf (xml-tag-children x)
          (list value))))
