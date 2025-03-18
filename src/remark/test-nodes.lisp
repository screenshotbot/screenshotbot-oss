(defpackage :remark/test-nodes
  (:use #:cl
        #:fiveam)
  (:import-from #:remark/nodes
                #:*nodes*)
  (:local-nicknames (#:a #:alexandria)))
(in-package :remark/test-nodes)

(def-suite :remark)
(def-suite* :remark/test-nodes :in :remark)

(def-fixture state ()
  (let ((*nodes* (make-hash-table)))
    (&body)))

(test preconditions
  (with-fixture state ()
    (unwind-protect
         (pass)
      (setf (symbol-value '*dummy-docs*)  nil))))

(test defpage
  (with-fixture state ()
    (remark:defpage foo ()
      <p>hello world</p>)
    (is-true (gethash 'foo *nodes*))
    (is-true (remark:find-node 'foo))))


(test defsection
  (with-fixture state ()
    (remark:defpage foo ()
      <p>hello world</p>)
    (remark:defsection bar ()
      foo)
    (is-true (remark:find-node 'bar))))

(test deftoplevel
  (with-fixture state ()
    (remark:deftoplevel *dummy-docs* ()
      foo)
    (is-true *dummy-docs*)))
