;; Copyright 2018-Present Modern Interpreters Inc.
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/dag/test-dag
  (:use #:cl
        #:dag
        #:fiveam
        #:fiveam-matchers)
  (:import-from #:dag
                #:%dfs-topo-sort-from-starting-nodes
                #:dfs-topo-sort
                #:*use-dfs-p*
                #:commit-timestamp
                #:all-commits
                #:dag-difference
                #:best-path
                #:ancestorp
                #:merge-base
                #:reachable-nodes
                #:assert-commit
                #:ordered-commits
                #:commit-map
                #:commit-node-id
                #:node-id
                #:safe-topological-sort
                #:dag
                #:merge-dag
                #:get-commit
                #:commit
                #:node-already-exists
                #:add-commit)
  (:import-from #:flexi-streams
                #:with-output-to-sequence)
  (:import-from #:fiveam-matchers/errors
                #:error-with-string-matching)
  (:import-from #:fiveam-matchers/core
                #:does-not
                #:has-typep
                #:has-any
                #:assert-that)
  (:import-from #:fiveam-matchers/lists
                #:contains-in-any-order
                #:contains)
  (:import-from #:alexandria
                #:assoc-value)
  (:import-from #:fiveam-matchers/strings
                #:contains-string))
(in-package :screenshotbot/dag/test-dag)

(util/fiveam:def-suite)

(def-fixture state ()
  (flet ((run-inner ()
           (let ((dag (make-instance 'dag)))
             (flet ((add-edge (from to &key (dag dag))
                      (add-commit dag (make-instance 'commit :sha from :parents (cond
                                                                                  ((listp to)
                                                                                   to)
                                                                                  (t (list to)))
                                                             :author "Arnold Noronha <arnold@tdrhq.com>"))))
               (add-edge "bb" nil)
               (add-edge "aa" "bb")
               (&body)))))
    (let ((dag::*use-dfs-p* nil))
      (run-inner))
    (let ((dag::*use-dfs-p* t))
      (run-inner))))

(defun make-big-linear-dag (&key (num 100))
  (let* ((dag (make-instance 'dag))
         (commits (loop for i from 1 to num collect
                                            (format nil "~4,'0X" i)))
         (fns nil))
    (flet ((add-edge (from to)
             (add-commit dag (make-instance 'commit :sha from :parents (cond
                                                                         ((listp to)
                                                                          to)
                                                                         (t (list to)))))))
      (loop for to in commits
            for from in (cdr commits)
            do
               (let ((to to) (from from))
                 (push
                  (lambda ()
                    (add-edge from to))
                  fns)))

      (add-edge (car commits) nil))

    (loop for fn in (reverse fns)
          do (funcall fn))

    (values dag commits)))

(defmacro pp (x)
  `(let ((y ,x))
     (log:info "Got ~S for ~S" y ',x)
     y))

(test preconditions
  (with-fixture state ()
    (pass)))

(test wont-let-me-add-cycles
  (with-fixture state ()
    (signals node-already-exists
      (add-edge "bb" nil))))

(test serialize
  (with-fixture state ()
    (let ((json (with-output-to-string (s)
                  (write-to-stream dag s))))
      (is (equal `(((:sha . "bb")
                    (:author . "Arnold Noronha <arnold@tdrhq.com>")
                    (:parents . nil))
                   ((:sha . "aa")
                    (:author . "Arnold Noronha <arnold@tdrhq.com>")
                    (:parents . ,(list "bb"))))
                 (assoc-value (json:decode-json-from-string json)
                              :commits))))))

(test technically-we-can-still-create-a-cyclical-graph
  (with-fixture state ()
    (add-edge "cc" "dd")
    (finishes
     (add-edge "dd" "cc"))))

(test serialize-incomplete-graph
  (with-fixture state ()
    (add-edge "cc" (list "aa" "dd"))
    (let ((json (with-output-to-string (s)
                  (write-to-stream dag s))))
      (assert-that
       (assoc-value (json:decode-json-from-string json)
                    :commits)
       (apply #'contains-in-any-order
        `(((:sha . "bb")
           (:author . "Arnold Noronha <arnold@tdrhq.com>")
           (:parents . nil))
          ((:sha . "aa")
           (:author . "Arnold Noronha <arnold@tdrhq.com>")
           (:parents . ,(list "bb")))
          ((:sha . "cc")
           (:author . "Arnold Noronha <arnold@tdrhq.com>")
           (:parents . ,(list "aa" "dd"))))
        ))
      (let ((dag (read-from-stream (make-string-input-stream json))))
        (is (typep dag 'dag))))))

(test serialize-binary
  (with-fixture state ()
    (let ((output (flex:with-output-to-sequence (s)
                    (write-to-stream dag s :format :binary))))
      (let ((read-dag
              (read-from-stream (flex:make-in-memory-input-stream output)
                                :format :binary)))
        (is (typep read-dag 'dag))))))

(test read
  (with-fixture state ()
    (let ((json (with-output-to-string (s)
                  (write-to-stream dag s))))
      (let ((dag (read-from-stream (make-string-input-stream json))))
        (is (equal `(((:sha . "bb")
                      (:author . "Arnold Noronha <arnold@tdrhq.com>")
                      (:parents . nil))
                     ((:sha . "aa")
                      (:author . "Arnold Noronha <arnold@tdrhq.com>")
                      (:parents . ,(list "bb"))))
                   (assoc-value (json:decode-json-from-string
                                 (with-output-to-string (s)
                                   (write-to-stream dag s)))
                                :commits)))))))

(test merge-dag
  (with-fixture state ()
    (let ((to-dag (make-instance 'dag)))
      (merge-dag to-dag dag)
      (is-true (get-commit to-dag "aa"))
      (is-true (get-commit to-dag "bb"))
      (uiop:with-temporary-file (:stream s :pathname p)
        (dag:write-to-stream to-dag s)
        (with-open-file (input p :direction :input)
          (let ((re-read (dag:read-from-stream input)))
            (is-true (get-commit re-read "aa"))
            (is-true (get-commit re-read "bb"))))))))

(test big-dag-topological-sort
  (multiple-value-bind (dag commits) (make-big-linear-dag)
    (let ((final-order (loop for x in (assoc-value
                                       (json:decode-json-from-string
                                        (with-output-to-string (s)
                                          (write-to-stream dag s)))
                                       :commits)
                             collect (assoc-value x :sha))))
      (is (equal final-order commits)))))

(test dfs-topo-sort-recursion
  (multiple-value-bind (dag commits) (make-big-linear-dag :num #xf000)
    (let ((start-commit (get-commit dag "F000")))
      (is (not (null start-commit)))
      (%dfs-topo-sort-from-starting-nodes
       dag
       (list start-commit))
      (pass))
    (finishes
      (dfs-topo-sort dag))))

(test merge-existing-commits
  (with-fixture state ()
   (let ((dag1 (make-instance 'dag))
         (dag2 (make-instance 'dag)))
     (add-edge "bb" nil :dag dag1)
     (add-edge "bb" nil :dag dag2)
     (add-edge "aa" "bb" :dag dag1)
     (add-edge "cc" "bb" :dag dag2)
     (merge-dag dag1 dag2))))

(test document-the-right-order-topo-sort
  "Oldest first? Or newest first? You'll always wonder, so here's a test
for you."
  (with-fixture state ()
    (let ((dag (make-instance 'dag)))
      (flet ((find-commit (id)
               (gethash id (commit-map dag))))
        (add-edge "bb" nil :dag dag)
        (add-edge "aa" "bb" :dag dag)
        #+nil
        (assert-that (car (safe-topological-sort dag))
                     (has-typep 'commit))
        (let ((commits (mapcar #'sha (mapcar #'find-commit (safe-topological-sort dag)))))
          (assert-that commits
                       (contains "aa" "bb")))
        (add-edge "cc" "bb" :dag dag)
        (add-edge "dd" (list "aa" "cc") :dag dag)
        (let ((commits (mapcar #'sha (mapcar #'find-commit (safe-topological-sort  dag)))))
          (assert-that commits
                       (has-any
                        (contains "dd" "cc" "aa" "bb" )
                      (contains "dd" "aa" "cc" "bb" ))))))))


(test document-the-right-order-topo-sort-long-names
  "Oldest first? Or newest first? You'll always wonder, so here's a test
for you."
  (flet ((ll (name)
           (let ((name (str:join "" (list name name))))
            (str:join ""
                      (list name name name name name
                            name name name name name
                            name name name name name
                            name name name name name)))))
    (assert (not (typep (commit-node-id (ll "aa")) 'fixnum)))
    (with-fixture state ()
      (let ((dag (make-instance 'dag)))
       (flet ((find-commit (id)
                (gethash id (commit-map dag))))
         (add-edge (ll "bb") nil :dag dag)
         (add-edge (ll "aa") (ll "bb") :dag dag)
         (let ((commits (mapcar #'sha (mapcar #'find-commit (safe-topological-sort dag)))))
           (assert-that commits
                        (contains (ll "aa") (ll "bb"))))
         (add-edge (ll "cc") (ll "bb") :dag dag)
         (add-edge (ll "dd") (list (ll "aa") (ll "cc")) :dag dag)
         (let ((commits (mapcar #'sha (mapcar #'find-commit (safe-topological-sort dag)))))
           (assert-that commits
                        (contains (ll "dd") (ll "cc") (ll "aa") (ll "bb") ))))))))

(test topo-sort-on-incomplete-graph
  (with-fixture state ()
    (let ((dag (make-instance 'dag)))
      (flet ((find-commit (id)
               (gethash id (commit-map dag))))
        (add-edge "aa" "bb" :dag dag)
        (let ((commits (mapcar #'sha (mapcar #'find-commit (safe-topological-sort dag)))))
          (assert-that commits
                       (contains "aa")))))))

(test merge-incomplete-graph-to-empty-graph
  (with-fixture state ()
    (let ((dag (make-instance 'dag))
          (old-dag (make-instance 'dag)))
      (flet ((find-commit (id)
               (gethash id (commit-map old-dag))))
        (add-edge "aa" "bb" :dag dag)
        (merge-dag old-dag dag)
        (let ((commits (mapcar #'sha (mapcar #'find-commit (safe-topological-sort old-dag)))))
          (assert-that commits
                       (contains "aa")))))))

(test ordered-commits
  "This is being called from the SDK"
  (with-fixture state ()
    (let ((dag (make-instance 'dag)))
      (add-edge "aa" "bb" :dag dag)
      (assert-that (mapcar #'sha (ordered-commits dag))
                   (contains "aa")))))

(test assert-commit
  (handler-case
      (progn
        (assert-commit "foo" "foo bar")
        (fail "expected error"))
    (error (e)
      (assert-that (format nil "~a" e)
                   (contains-string "`foo` does not")))))

(test reachable-nodes
  (with-fixture state ()
    (let ((dag (make-instance 'dag)))
      (add-edge "bb" nil :dag dag)
      (add-edge "aa" "bb" :dag dag)
      (assert-that
       (mapcar #'sha
               (reachable-nodes dag "aa"))
       (has-item "aa")
       (has-item "bb"))
      (assert-that
       (mapcar #'sha
               (reachable-nodes dag "bb"))
       (contains "bb"))

      (add-edge "dd" "aa" :dag dag)
      (assert-that
       (mapcar #'sha
               (reachable-nodes dag "dd"))
       (has-item "bb"))
      (assert-that
       (mapcar #'sha
               (reachable-nodes dag "dd" :depth 2))
       (is-not (has-item "bb"))
       (has-item "aa")
       (has-length 2)))))

(test we-get-callbacks-for-nodes-that-arent-present
  (with-fixture state ()
    (let ((dag (make-instance 'dag)))
      (add-edge "cc" "dd" :dag dag)

      (let ((seen nil))
        (reachable-nodes dag "cc"
                         :seen-callback (lambda (commit)
                                          (push (sha commit) seen)))
        (assert-that seen
                     (contains "dd" "cc"))))))

(test we-get-callbacks-for-nodes-that-arent-present--but-only-once!
  (with-fixture state ()
    (let ((dag (make-instance 'dag)))
      (add-edge "cc" "dd" :dag dag)
      (add-edge "ee" (list "cc" "dd") :dag dag)

      (let ((seen nil))
        (reachable-nodes dag "ee"
                         :seen-callback (lambda (commit)
                                          (push (sha commit) seen)))
        (assert-that seen
                     (contains "dd" "cc" "ee"))))))

(def-fixture F279319 ()
  (add-edge "cc" "bb" :dag dag)
  (is (equal "bb" (merge-base dag "aa" "cc")))
  (add-edge "dd" "ee")
  ;; At this point, the graph looks like: https://phabricator.tdrhq.com/F279319
  (&body))

(test merge-base
  (with-fixture state ()
    (with-fixture F279319 ()
      (is (equal nil (merge-base dag "aa" "dd")))
      (is (equal "aa" (merge-base dag "aa" "aa" :exclude-commit-2 nil)))
      (is (equal "aa" (merge-base dag "aa" "aa" :exclude-commit-2 t))))))

(test merge-base-allows-multiple-commit-1s
  (with-fixture state ()
    (with-fixture F279319 ()
      (is (equal "bb" (merge-base dag (list "aa" "dd") "cc"))))))

(test merge-base-allows-completely-invalid-commits
  (with-fixture state ()
    (with-fixture F279319 ()
      (is (equal "bb" (merge-base dag (list "aa" "ff") "cc")))
      (is (equal nil (merge-base dag "ff" "cc")))
      (is (equal nil (merge-base dag nil "cc"))))))

(test merge-base-v2
  (with-fixture state ()
    (add-edge "cc" "bb" :dag dag)
    (is (equal "bb" (merge-base dag "aa" "cc")))
    (add-edge "dd" "ee")
    ;; At this point, the graph looks like: https://phabricator.tdrhq.com/F279319
    (is (equal nil (merge-base dag "aa" "dd")))
    (is (equal "aa" (merge-base dag "aa" "aa" :exclude-commit-2 nil)))
    (is (equal "aa" (merge-base dag "aa" "aa" :exclude-commit-2 t)))))

(test merge-base-finds-*greatest*-common-ancestor-not-just-any-ancestor
  (with-fixture state ()
    ;; This is the graph we're creating: https://phabricator.tdrhq.com/F279321
    (let ((dag (make-instance 'dag:dag)))
      (add-edge "aa" "bb" :dag dag)
      (add-edge "bb" "cc" :dag dag)
      (add-edge "cc" "ff" :dag dag)
      (add-edge "ff" "11" :dag dag)
      (add-edge "ee" (list "dd" "ff") :dag dag)
      (add-edge "dd" "cc" :dag dag)
      (is (equal "cc" (merge-base dag "aa" "ee")))
      (is (equal "cc" (merge-base dag "ee" "aa"))))))

(test merge-base-finds-*greatest*-common-ancestor-not-just-any-ancestor-2
  (with-fixture state ()
    ;; This is the graph we're creating:
    ;; https://phabricator.tdrhq.com/F279320 This is almost like the
    ;; above one, but one extra node in the history, which showed up
    ;; as a bug when writing the previous one.
    (let ((dag (make-instance 'dag:dag)))
      (add-edge "aa" "bb" :dag dag)
      (add-edge "bb" "cc" :dag dag)
      (add-edge "cc" "ff" :dag dag)
      (add-edge "ee" (list "dd" "ff") :dag dag)
      (add-edge "dd" "cc" :dag dag)
      (is (equal "cc" (merge-base dag "aa" "ee")))
      (is (equal "cc" (merge-base dag "ee" "aa"))))))

(test there-can-be-multiple-greatest-common-ancestors
  (with-fixture state ()
    ;; This is the graph we're creating:
    ;; https://phabricator.tdrhq.com/F279382
    (let ((dag (make-instance 'dag:dag)))
      (add-edge "aa" (list "bb" "33") :dag dag)
      (add-edge "bb" "cc" :dag dag)
      (add-edge "cc" "ff" :dag dag)
      (add-edge "ff" "11" :dag dag)
      (add-edge "ee" (list "dd" "33") :dag dag)
      (add-edge "33" "11" :dag dag)
      (add-edge "dd" "cc" :dag dag)

      (assert-that
       (nth-value 1 (merge-base dag "aa" "ee"))
       (contains-in-any-order "cc" "33"))
      (assert-that
       (nth-value 1 (merge-base dag "ee" "aa"))
       (contains-in-any-order "cc" "33")))))

(defun set-timestamp (dag sha ts)
  (let ((commit (get-commit dag sha)))
    (setf (commit-timestamp commit) ts)))

(test there-can-be-multiple-greatest-common-ancestors--but-we-always-prefer-the-later-one
  (with-fixture state ()
    ;; This is the graph we're creating:
    ;; https://phabricator.tdrhq.com/F279382
    (let ((dag (make-instance 'dag:dag)))
      (add-edge "aa" (list "bb" "33") :dag dag)
      (add-edge "bb" "cc" :dag dag)
      (add-edge "cc" "ff" :dag dag)
      (add-edge "ff" "11" :dag dag)
      (add-edge "ee" (list "dd" "33") :dag dag)
      (add-edge "33" "11" :dag dag)
      (add-edge "dd" "cc" :dag dag)

      (set-timestamp dag "33" 101)
      (set-timestamp dag "cc" 100);

      (assert-that
       (nth-value 1 (merge-base dag "aa" "ee"))
       (contains "33" "cc"))
      (is (equal "33" (merge-base dag "aa" "ee")))
      (assert-that
       (nth-value 1 (merge-base dag "ee" "aa"))
       (contains "33" "cc"))
      (is (equal "33" (merge-base dag "ee" "aa"))))))

(test there-can-be-multiple-greatest-common-ancestors--but-we-always-prefer-the-later-one-flipped
  (with-fixture state ()
    ;; This is the graph we're creating:
    ;; https://phabricator.tdrhq.com/F279382
    (let ((dag (make-instance 'dag:dag)))
      (add-edge "aa" (list "bb" "33") :dag dag)
      (add-edge "bb" "cc" :dag dag)
      (add-edge "cc" "ff" :dag dag)
      (add-edge "ff" "11" :dag dag)
      (add-edge "ee" (list "dd" "33") :dag dag)
      (add-edge "33" "11" :dag dag)
      (add-edge "dd" "cc" :dag dag)

      (set-timestamp dag "33" 100)
      (set-timestamp dag "cc" 101);

      (assert-that
       (nth-value 1 (merge-base dag "aa" "ee"))
       (contains "cc" "33"))
      (is (equal "cc" (merge-base dag "aa" "ee")))
      (assert-that
       (nth-value 1 (merge-base dag "ee" "aa"))
       (contains "cc" "33"))
      (is (equal "cc" (merge-base dag "ee" "aa"))))))

(test excludes-commit-2-when-finding-merge-base
  (with-fixture state ()
    (let ((dag (make-instance 'dag)))
      (flet ((add (a &rest b)
               (add-edge a b :dag dag)))
        ;; This is the graph: https://phabricator.tdrhq.com/F279537
        ;; We're trying to merge "ee" into "22"
        (add "22" "aa")
        (add "aa" "bb" "ee")
        (add "bb" "cc")
        (add "ee" "dd" "33")
        (add "dd" "cc")
        (add "cc" "ff")
        (add "ff" "11"))
      (is (equal "cc" (merge-base dag "22" "ee"))))))

(test cannot-exclude-commit-2-when-commit-2-all-paths-go-through-commit-2
  (with-fixture state ()
    (let ((dag (make-instance 'dag)))
      (flet ((add (a &rest b)
               (add-edge a b :dag dag)))
        ;; This is the graph: https://phabricator.tdrhq.com/F279537
        ;; We're trying to merge "ff" into "22"
        (add "22" "aa")
        (add "aa" "bb" "ee")
        (add "bb" "cc")
        (add "ee" "dd" "33")
        (add "dd" "cc")
        (add "cc" "ff")
        (add "ff" "11")
        ;; This is the case we actually care about:
        (is (equal "ff" (merge-base dag "22" "ff")))

        ;; But this should also hold true,
        (is (equal "ff" (merge-base dag "ff" "22")))        

        (add "44" "55")
        ;; But let's also test the case where it's not an ancestor
        (is (equal nil (merge-base dag "22" "44")))))))

(test ancestorp
  (with-fixture state ()
    (add-edge "cc" "aa" :dag dag)
    (is (ancestorp dag "bb" "cc"))
    (is (ancestorp dag "cc" "cc"))
    (is (ancestorp dag "aa" "cc"))
    ;; What if the commit doesn't exist at all?
    (is (ancestorp dag "a1" "a1"))
    (is (not (ancestorp dag "cc" "bb")))))

(def-fixture best-path ()
  (add-edge "cc" (list "ff" "ee") :dag dag)
  (add-edge "ff" "dd" :dag dag)
  (add-edge "dd" (list "ee" "11") :dag dag)
  (add-edge "22" "33")
  (&body))

(test best-path
  (with-fixture state ()
    (with-fixture best-path ()
     (assert-that (best-path dag "cc" "ee")
                  (contains "cc" "ff" "dd" "ee")))))

(test best-path-with-max-depth
  (with-fixture state ()
   (with-fixture best-path ()
     (assert-that (best-path dag "cc" "ee" :max-depth 1)
                  (contains))
     (assert-that (best-path dag "cc" "ee" :max-depth 2)
                  (described-as "Even though there's a longer 'better-path' the max-depth means we'll use the smaller path"
                    (contains "cc" "ee")))
     (assert-that (best-path dag "cc" "11" :max-depth 2)
                  (described-as "Even though there's a longer 'better-path' the max-depth means we'll use the smaller path"
                    (contains))))))

(test best-path-when-theres-not-path
  (with-fixture state ()
    (with-fixture best-path ()
      (assert-that (best-path dag "cc" "33")
                   (contains)))))

(test best-path-when-original-commit-is-not-present
  (with-fixture state ()
    (with-fixture best-path ()
      (assert-that (best-path dag "44" "55")
                   (contains))
      (assert-that (best-path dag "44" "33")
                   (contains)))))

(test dag-difference
  (with-fixture state ()
    (let ((other (make-instance 'dag)))
      (add-edge "aa" "bb" :dag other)
      (add-edge "dd" "ee" :dag other)
      (let ((result (dag:dag-difference dag other)))
        (assert-that (mapcar #'sha (all-commits result))
                     (has-item "bb")
                     (does-not (has-item "aa"))
                     (does-not (has-item "dd"))
                     (has-length 1))))))
