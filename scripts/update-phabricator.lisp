(require :asdf)

(ql:quickload :util/phabricator)

(defpackage :my-test
  (:use :cl
   :util/phabricator/conduit))
(in-package :my-test)

(let ((phid (uiop:getenv "PHID")))
  (let ((phab (make-phab-instance-from-arcrc "https://phabricator.tdrhq.com")))
    (when phid
      (let* ((args `(("buildTargetPHID" . ,(uiop:getenv "PHID"))
                     ("type" . ,(if (member "pass" (uiop:raw-command-line-arguments)
                                            :test 'string=)
                                    "pass"
                                    "fail")))))
        (call-conduit
         phab
         "harbormaster.sendmessage"
         args)))))
