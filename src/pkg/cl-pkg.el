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
  (flet ((start-of-next-sexp
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
                      (return nil)))))
              finally
              (progn
                ;; if we're here, we didn't find the appropriate import from :/
                (forward-sexp 1)
                (insert (format "\n(:import-from %s)" import-from))
                (funcall indent-line-function)
                (backward-sexp 1))))))

(defun cl-pkg-add-import (name to)
  (cl-pkg--jump-to-define-package)
  (cl-pkg--jump-to-import-from to))
