
(defun cl-pkg-search-buffer-package ()
  (or
   (let ((case-fold-search t)
         (regexp (concat "^[ \t]*(\\(pkg:\\)?define-package\\>[ \t']*"
                         "\\([^ )]+\\)[ \t]*$")))
     (save-excursion
       (when (or (re-search-backward regexp nil t)
                 (re-search-forward regexp nil t))
         (match-string-no-properties 2))))
   (slime-search-buffer-package)))

(setf slime-find-buffer-package-function 'cl-pkg-search-buffer-package)
