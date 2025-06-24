(defpackage :remark/nodes
  (:use #:cl)
  (:import-from #:util/html2text
                #:html2text)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:defpage
   #:find-node
   #:defsection
   #:deftoplevel
   #:toplevel-prefix
   #:section-children
   #:section-title
   #:section
   #:toplevel
   #:section-id
   #:locate-page
   #:page-generator
   #:*toplevel*
   #:toplevel-landing-page
   #:locate-page-by-alias
   #:page-name))
(in-package :remark/nodes)

(defvar *nodes* (make-hash-table))

(defclass node ()
  ((staging? :initarg :staging?
             :initform nil
             :reader only-show-on-staging?)
   (default :initform nil
            :initarg :default
            :reader page-default-p)))

(defclass page (node)
  ((fn :initarg :fn
     :reader page-generator)
   (name :initarg :name
         :reader page-name)
   (title :initarg :title
          :initform "Untitled page"
          :reader section-title)
   (aliases :initarg :aliases
            :initform nil
            :reader page-aliases)
   (text :initform nil
         :accessor %page-text
         :documentation "A cached version of the text of the page")))

(defclass section (node)
  ((title :initarg :title
          :initform "Untitled"
          :reader section-title)
   (children :initarg :children
             :reader section-children)))

(defmethod section-id (page-or-section)
  (str:replace-all " " "-"
   (string-downcase (section-title page-or-section))))

(defclass toplevel (section)
  ((prefix :initarg :prefix
           :reader toplevel-prefix)
   (landing-page :initarg :landing-page
                 :reader toplevel-landing-page)))

(defvar *toplevel* nil
  "The top level context. Bound when calling most of the methods
  related to remarks.")

(defmacro defpage (name args &body body)
  `(setf
    (gethash ',name *nodes*)
    (make-instance 'page
                   :name ',name
                   :fn (lambda () ,@body)
                   ,@args)))

(defmethod initialize-instance :after ((page page) &key aliases &allow-other-keys)
  (assert (listp aliases))
  (loop for alias in aliases
        do (assert (stringp alias))))


(defmacro defsection (name args &body body)
  `(setf
    (gethash ',name *nodes*)
    (make-instance 'section
                    :children
                    ',body
                     ,@args)))

(defmacro deftoplevel (name (&rest args) &body body)
  `(defparameter ,name
     (make-instance 'toplevel
                     :children ',body
                     ,@args)))


(defun find-node (name)
  (gethash name *nodes*))

(defmethod locate-page ((section section) path)
  (assert path)
  (destructuring-bind (name &optional rest) (str:split "/" path :limit 2)
    (cond
      ((and (str:emptyp name) rest)
       (locate-page section rest))
      ((str:emptyp name)
       ;; find a default page
       (loop for child-name in (section-children section)
             for node = (find-node child-name)
             if (page-default-p node) do
               (return (locate-page node ""))
             finally
              (return section)))
      (t
       (loop for child-name in (section-children section)
             for child = (find-node child-name)
             if (string= (section-id child) name)
               return
               (if rest
                   (locate-page child rest)
                   (values child section)))))))

(defmethod locate-page ((section page) path)
  (when (str:emptyp path)
    section))

(defmethod locate-page ((toplevel toplevel) path)
  (cond
    ((str:starts-with-p (toplevel-prefix toplevel) path)
     (call-next-method toplevel
                       (str:substring (length (toplevel-prefix toplevel)) nil
                                      path)))
    (t
     (call-next-method))))

(defmethod locate-page-by-alias ((node node) path)
  (loop for child-name in (section-children node)
        for child = (find-node child-name)
        for page = (locate-page-by-alias child path)
        if page
          return page))

(defmethod locate-page-by-alias ((page page) path)
  (when (str:s-member (page-aliases page) path)
    page))

(defmethod page-text ((page page))
  (util/misc:or-setf
   (%page-text page)
   (str:downcase
    (str:join
     " "
     (loop for line in (str:lines
                        (html2text
                         (funcall (page-generator page))))
           collect
           (cl-ppcre:regex-replace-all "][(].*[)]"
                                       (cl-ppcre:regex-replace-all "^[!]\\[\\][(].*[)]"
                                                                   line
                                                                   "")
                                       ""))))))


