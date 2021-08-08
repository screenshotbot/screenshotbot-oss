;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(uiop:define-package :quick-patch/util
    (:use #:cl))
(in-package :quick-patch/util)

;;;; from cl-str

(defvar *whitespaces* '(#\Space #\Newline #\Backspace #\Tab
                        #\Linefeed #\Page #\Return #\Rubout))

(defun trim (value)
  (string-trim *whitespaces* value))

;;;; end: from cl-str

;;;; from cl-fad (with slight modifications)

(defun catdir (x y)
  (let ((x (pathname x))
        (y (pathname y)))
    (assert (eql :relative (car (pathname-directory y))))
    (make-pathname
     :directory (cons
                 (car (pathname-directory x))
                 (append (cdr (pathname-directory x)) (cdr (pathname-directory y))))
     :defaults x)))

(defun directory-exists-p (pathspec)
  "Checks whether the file named by the pathname designator PATHSPEC
exists and if it is a directory.  Returns its truename if this is the
case, NIL otherwise.  The truename is returned in directory form as if
by PATHNAME-AS-DIRECTORY."
  #+:allegro
  (and (excl:probe-directory pathspec)
       (truename pathspec))
  #+:lispworks
  (and (lw:file-directory-p pathspec)
       (truename pathspec))
  #-(or :allegro :lispworks)
  (let ((result (probe-file pathspec)))
    (and result
         (not (pathname-name result)))))

;;;; end: from cl-fad
