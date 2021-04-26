(in-package :util)

(markup:enable-reader)

(defun html2text (markup)
  (let* ((markup (if (stringp markup) markup
                     (markup:write-html markup)))
         (input-stream (make-string-input-stream markup))
         (output-stream (make-string-output-stream)))
    (uiop:run-program (list "html2text")
                      :input input-stream
                      :output output-stream)
    (get-output-stream-string output-stream)))
