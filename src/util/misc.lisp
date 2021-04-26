(in-package :util)

(defun supports-webp? ()
  (if (boundp 'hunchentoot:*request*)
      (str:contains?  "image/webp" (hunchentoot:header-in :accept hunchentoot:*request*))))

(defun fix-for-webp (url &key force)
  (cond
    ((or force (supports-webp?))
     (let ((parts (str:split "?" url)))
       (str:join "?" (cons
                      (ppcre:regex-replace "\\.[a-z]*$" (car parts) ".webp")
                      (cdr parts)))))
    (t
     url)))
