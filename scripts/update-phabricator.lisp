(require :asdf)

(ql:quickload :cl-json)



(let ((phid (uiop:getenv "PHID")))
  (format t "Got PHID: ~A~%" phid)
  (when phid
   (let* ((args `(("buildTargetPHID" . ,(uiop:getenv "PHID"))
                  ("type" . ,(if (member "pass" (uiop:raw-command-line-arguments)
                                         :test 'string=)
                                 "pass"
                                 "fail")))))
     (uiop:run-program (list "arc" "call-conduit" "harbormaster.sendmessage")
                       :input (make-string-input-stream (cl-json:encode-json-to-string args))))))
