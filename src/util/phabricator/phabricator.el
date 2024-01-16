(provide 'differential)

(define-derived-mode differential-mode tabulated-list-mode
  "Phabricator differential list"
  ""
  (setq tabulated-list-format
        [("Revision" 5 t)
         ("Branch" 10 t)
         ("Status" 10 t)
         ("Build Status" 10 t)
         ("Title" 10 t)]))

(defun differential--buffer ()
  (get-buffer-create "*phabricator-revisions*"))

(defun differential--good (message)
  "The constant PASS with font-face."
  (propertize message
              :foreground "green"))


(defun differential--bad (message)
  "The constant FAIL with font-face."
  (propertize message
              :background  "red"
              :foreground  "white"
              :weight "bold"))

(defun differential-query-all ()
  (let ((response (maniphest-call-conduit
                   "differential.query"
                   (let ((dict (make-hash-table)))
                     (puthash "status" "status-open" dict)
                     dict))))
    (gethash
     "response"
     response)))

(defun differential--format-status (status)
  (cond
   ((equal "Draft" status)
    (differential--good status))
   (t
    (differential--bad status))))

(defun differential--build-status (revision)
  (let ((buildables (gethash "buildables" (gethash "properties" revision))))
    (cond
     (buildables
      (let ((failed 0)
            (waiting 0)
            (passed 0))
        (cl-loop for res being the hash-values of buildables
                 for status = (gethash "status" res)
                 if (equal "failed" status)
                 do (incf failed)
                 if (equal "pending" status)
                 do (incf waiting)
                 if (equal "passed" status)
                 do (incf passed))
        (cond
         ((> failed 0)
          "failed")
         ((> waiting 0)
          "waiting")
         ((> passed 0)
          "passed")
         (t
          "unknown"))))
     (t
      "No buildables"))))

(defun differential-revision-list ()
  (interactive)
  (let ((buffer (differential--buffer)))
    (with-current-buffer buffer
      (differential-mode)
      (setq tabulated-list-entries
            (cl-loop for rev across (differential-query-all)
                     for i from 1
                     collect
                     (list (gethash "id" rev)
                           (vector
                            (format "D%s" (gethash "id" rev))
                            (gethash "branch" rev)
                            (differential--format-status (gethash "statusName" rev))
                            (differential--build-status rev)
                            (gethash "title" rev)))))
      (tabulated-list-init-header)
      (tabulated-list-print)
      (display-buffer buffer))))
