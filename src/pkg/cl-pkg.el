;; coding: -*- lexical-binding: t -*-

(defun cl-pkg-search-buffer-package ()
  (or
   (let ((case-fold-search t)
         (regexp (concat "^[ \t]*(\\(pkg:\\)?define-package\\>[ \t']*"
                         "\\([^ )]+\\)[ \t]*$")))
     (save-excursion
       (when (or (re-search-backward regexp nil t)
                 (re-search-forward regexp nil t))
         (match-string-no-properties 2))))
   (sly-search-buffer-package)))

(setf sly-find-buffer-package-function 'cl-pkg-search-buffer-package)

(defun cl-pkg--jump-to-define-package ()
  (interactive)
  (goto-char 0)
  (re-search-forward "(defpackage\\|(pkg:define-package\\|(uiop:define-package")
  (beginning-of-line))

(defun cl-pkg--jump-to-import-from (import-from)
  (interactive)
  (cl-flet ((start-of-next-sexp
          ()
          (ignore-errors
            (forward-sexp 2)
            (backward-sexp 1)
            t)))

    (forward-char)
    (forward-sexp 1)

    ;; I'm at the first argument of defpackage
    (or
     (cl-loop while (start-of-next-sexp)
              do
              (progn
                (when(string-prefix-p "(:import-from" (thing-at-point 'sexp))
                  (message "at: %s" (thing-at-point 'sexp))
                  (save-excursion
                    (forward-char)
                    (forward-sexp 2)
                    (when (equal import-from (thing-at-point 'sexp))
                      ;; we're here
                      (cl-return nil)))))
              finally
              (progn
                ;; if we're here, we didn't find the appropriate import from :/
                (forward-sexp 1)
                (insert (format "\n(:import-from %s)" import-from))
                (funcall indent-line-function)
                (backward-sexp 1))))))

(defun cl-pkg--insert-tail (name)
  "Insert the name to the tail of the current sexp (we're at the opening parenthesis"
  (forward-sexp)
  (backward-char)
  (insert "\n")
  (insert name)
  (funcall indent-line-function))

(defun cl-pkg--reorder-imports ()
  (save-excursion
    (indent-pp-sexp)
    (let ((end (- (save-excursion (end-of-sexp)
                                  (point)) 1)))
      (save-excursion
        (goto-char end)
        (insert "\n"))
      (forward-char)
      (forward-sexp 2)
      (forward-line)
      (sort-lines nil (point) end)))
  ;; finally remove any newlines before the end of the sexp
  (forward-sexp)
  (backward-char 2)

  (delete-char 1))

(defun cl-pkg--format-import (x)
  (cond
   ((string-prefix-p "#:" x)
    x)
   (t
    (format "#:~s" x))))

(defvar cl-pkg--add-import-package-history nil)

(defun cl-pkg-add-import (name to)
  (interactive
   (list
    (read-from-minibuffer "Symbol:"
                          (thing-at-point 'sexp))
    (read-from-minibuffer "Package:"
                          nil
                          nil
                          'cl-pkg--add-import-package-history)))
  (save-excursion
    (cl-pkg--jump-to-define-package)
    (cl-pkg--jump-to-import-from to)
    (save-excursion
      (cl-pkg--insert-tail name))
    (save-excursion
      (cl-pkg--reorder-imports))))


(defvar sly-defpackage-regexp
  "^(\\(cl:\\|common-lisp:\\|pkg:\\|uiop:\\|\\uiop/package:\\)?\\(defpackage\\|define-package\\)\\>[ \t']*")
