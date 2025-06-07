;; Copyright 2018-Present Modern Interpreters Inc.
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(in-package :dag)

(define-condition node-already-exists (error)
  ())

(defvar *magic* (flex:string-to-octets "bdag" :external-format :latin1))

(defclass commit (encodable)
  ((sha :initarg :sha
        :accessor sha)
   (author :initarg :author
           :initform nil
           :accessor author)
   (ts :initarg :ts
       :initform 0
       :accessor commit-timestamp
       :documentation "The timestamp this commit was first seen")
   (parents :initarg :parents
            :initform nil
            :accessor parents
            :documentation "array of shas"))
  (:default-initargs :ts (get-universal-time)))

(defmacro pp (x)
  `(let ((y ,x))
     ;;(log:info "Got ~S for ~S" y ',x)
     y))

(defun commit-node-id (x)
  "The node id used for cl-graph queries."
  (check-type x string)
  (assert (> (length x) 0))
  (assert x)
  (let ((arr
          (ironclad:hex-string-to-byte-array x)))
    (let ((ret 0))
      (loop for i across arr do
        (setf ret
              (+ (ash ret 8) i)))
      ret)))

(defclass dag (encodable)
  ((commits :initform (make-hash-table :test 'equal)
            :initarg :commit-map
            :accessor commit-map
            :documentation "A map from COMMIT-NODE-ID (number) to COMMIT. We eventually plan to replace this with sha to COMMIT.")))

(defmethod clone-dag ((dag dag))
  "This is not bad in terms of performance! But obviously, we can optimize this in future with either FSET of CoW."
  (make-instance
   'dag
   :commit-map (alexandria:copy-hash-table (commit-map dag))))

(defmethod all-commits ((dag dag))
  (loop for commit being the hash-values of (commit-map dag)
        collect commit))

(defmethod ordered-commits ((dag dag))
  (let ((sorted (safe-topological-sort dag)))
    (loop for id in sorted
          collect (gethash id (commit-map dag)))))

(defmethod get-commit ((dag dag) (sha string))
  (let ((id (commit-node-id sha)))
    (log:debug "Commit id for ~a is ~a" sha id)
    (gethash id (commit-map dag))))

(defmethod bfs-search ((dag dag) start end)
  "This is technically an iterative DFS. We could replace this with
REACHABLE-NODES, or use a simple-queue instead of the stack, but for
now I'm avoiding this, because it can be hard to test edge cases of
this behavior in prod.. In the future, consider making either of these
changes."
  (let ((seen (make-hash-table :test 'equal))
        (stack nil))
    (setf (gethash start seen) t)
    (push start stack)
    (loop while stack do
      (let ((next (pop stack)))
        (when (equal (commit-node-id end)
                     (commit-node-id next))
          (return-from bfs-search t))
        (loop for parent in (alexandria:if-let (commit (get-commit dag next))
                              (parents commit)
                              (progn
                                (log:info "Commit ~a not in graph" next)
                                nil))
              do
                 (unless (gethash parent seen)
                   (setf (gethash parent seen) t)
                   (push parent stack)))))
    nil))

(defmethod ancestorp ((dag dag) (sha-old string) (sha-new string))
  (bfs-search
   dag
   sha-new
   sha-old))

(defmethod add-commit ((dag dag) (commit  commit))
  (assert commit)
  (let ((map (commit-map dag))
        (node-id (commit-node-id (sha commit))))
    (cond
      ((gethash node-id map)
       (error 'node-already-exists))
      (t
       (setf
        (gethash node-id map)
        commit)
       (loop for p in (parents commit) do
         (progn
           #+nil(log:info "adding edge from: ~S to ~S" node-id
                          (commit-node-id p))
           (assert (numberp node-id))
           (assert (numberp (commit-node-id p)))))))))

(defmethod full-commit-on-graph-p ((dag dag) node-id)
  "Not all nodes in the graph are \"complete\". Some nodes are referenced
by other nodes, but are not themselves available. This returns if the
node-id is a valid full node on the graph."
  (gethash node-id (commit-map dag)))

(defstruct stackframe
  sha
  commit
  children)

(defun %dfs-topo-sort-from-starting-nodes (dag start-nodes)
  "This function is mainly to make the starting of the DFS
deterministic, which makes it easier to test.

The reason this code is so hard to read: I first wrote a basic
recursive DFS. I then replaced it with a continuation passing style
version. Finally, I refactored it with a stack (where the stack is a
stack of lambdas, not the state itself)."
  (let ((visited (make-hash-table :test #'equal))
        (result nil)
        (stack nil))
    (labels ((dfs (sha callback)
               (block nil
                (let ((commit (get-commit dag sha)))
                  (cond
                    ((and commit (not (gethash commit visited)))
                     (setf (gethash commit visited) t)
                    
                     (let ((parents (dag:parents commit)))
                       (labels ((process-end ()
                                  (push sha result)
                                  (push callback stack))
                                (process-next-parent ()
                                  (cond
                                    (parents
                                     (let ((parent (pop parents)))
                                       (push
                                        (lambda ()
                                          (dfs parent #'process-next-parent))
                                        stack)))
                                    (t
                                     (process-end)))))
                         (process-next-parent))))
                    (t
                     (push callback stack)))))))

      (labels ((process-stack (start)
                 (push start stack)
                 (loop while stack
                       do
                          (let ((next (pop stack)))
                            (funcall next)))))
        (loop for commit in start-nodes
              do
                 (process-stack
                  (lambda ()
                    (?. dfs (dag:sha commit)  (lambda ())))))))
    (mapcar #'commit-node-id result)))

(defmethod dfs-topo-sort (dag)
  "This is modified from GRAPH. Sadly that library uses recursion for
some of their graph algorithms, which doesn't work nicely for a 'deep'
tree. This version uses the Kahn's algorithm instead of DFS

Returns the \"newest\" first and ancestors last. "
  ;;(declare (optimize (speed 3) (debug 0)))
  (%dfs-topo-sort-from-starting-nodes
   dag
   (loop for x being the hash-values of (commit-map dag)
         collect x)))

(defmethod kahns-algo-topo-sort (dag)
  (let* ((rL nil)
         (out-degrees (make-hash-table :test #'equal))
         (children (make-hash-table :test #'equal)))

    (loop for commit being the hash-values of (commit-map dag)
          do
             (setf (gethash (sha commit) out-degrees)
                   (length (parents commit)))
             (loop for parent in (parents commit) do
               ;; Ensure that parent nodes are present in here too
               (symbol-macrolet ((slot (gethash parent out-degrees)))
                 (unless slot
                   (setf slot 0)))

               ;; build precedents map
               (push (sha commit)
                     (gethash parent children nil))))

    (let ((S (loop for x being the hash-keys of out-degrees
                   if (eql 0 (gethash x out-degrees))
                     collect x)))
      (loop while S do
        (progn
          (let ((n (pop S)))
            (push n rL)
            (dolist (m (gethash n children))
              (decf (gethash m out-degrees))
              (assert (>= (gethash m out-degrees) 0))
              (when (eql (gethash m out-degrees) 0)
                (push m S)))))))
    (remove-if-not (curry #'full-commit-on-graph-p dag)
                   (mapcar #'commit-node-id rL))))

(defvar *use-dfs-p* nil
  "We needed the DFS version of topo sort just for rending the graph,
because it makes for a better human readable graph.

However, the Kahn's algorithm version is much better tested and I
don't want to switch it yet in the hot path. There are a bunch of
cases.")

(defmethod safe-topological-sort (dag &key (use-dfs-p *use-dfs-p*))
  "This is modified from GRAPH. Sadly that library uses recursion for
some of their graph algorithms, which doesn't work nicely for a 'deep'
tree. This version uses the Kahn's algorithm instead of DFS"
  (cond
    (use-dfs-p
     (dfs-topo-sort dag))
    (t
     (kahns-algo-topo-sort dag))))

(defmethod write-to-stream ((dag dag) stream &key (format :json))
  (let ((sorted-nodes (reverse (safe-topological-sort dag)))
        (commit-map (commit-map dag)))
    (ecase format
      (:json
       (json:encode-json
        `((:commits .
                    ,(loop for node-id in sorted-nodes
                           for commit = (gethash node-id commit-map)
                           if commit
                             collect
                           `((:sha . ,(sha commit))
                             (:author . ,(author commit))
                             (:parents . ,(parents commit)))))
          (:dummy . "0"))
        stream))
      (:binary
       (write-sequence
        *magic*
        stream)
       (write-byte 0 stream)
       (let ((version 1))
         (write-byte version stream))
       (encode-integer (length sorted-nodes) stream)
       (dolist (node-id sorted-nodes)
         (let ((commit (gethash node-id commit-map)))
           ;; TODO: can be further optimized to use integers, but this
           ;; should do for now
           (encode (sha commit) stream)
           (encode (author commit) stream)
           (encode (parents commit) stream))))))
  (finish-output stream))

(defun assert-commit (commit line)
  (unless (eql 40 (length commit))
    (error "`~a` does not look like a valid Git commit SHA1 string, read from `~a`"
           commit
           line)))


(defun read-from-stream (stream &key (format :json))
  (let ((dag (make-instance 'dag)))
    (ecase format
     (:json
      (let ((data (json:decode-json stream)))
        (loop for commit in (assoc-value data :commits) do
          (add-commit dag
                      (apply 'make-instance 'commit
                             (alist-plist commit))))
        dag))
     (:binary
      (let ((magic (make-array 4 :element-type '(unsigned-byte 8))))
        (read-sequence magic stream)
        (assert (equalp magic *magic*)))
      ;; version
      (read-byte stream)
      (let ((version (read-byte stream)))
        (assert (= 1 version)))
      (let ((length (decode stream)))
        (dotimes (i length)
          (let ((sha (decode stream))
                (author (decode stream))
                (parents (decode stream)))
            (add-commit dag
                        (make-instance 'commit
                                       :sha sha
                                       :author author
                                       :parents parents)))))
      dag)
     (:text
      (loop for line = (read-line stream nil)
            while (and line
                       (not (str:emptyp (str:trim line))))
            do
               (destructuring-bind (sha &rest parents) (str:split " " (str:trim line))
                 (assert-commit sha line)
                 (loop for parent in parents do
                   (assert-commit parent line))
                 (dag:add-commit dag (make-instance 'dag:commit
                                                    :sha sha
                                                    :parents parents))))
      dag))))

(defmethod merge-dag ((dag dag) (from-dag dag))
  (dolist (node-id (safe-topological-sort from-dag))
    (unless (gethash node-id (commit-map dag))
      (let ((commit (gethash node-id (commit-map from-dag))))
        (assert commit)
        (add-commit dag commit)
        (assert (gethash node-id (commit-map dag)))))))

(defmethod dag-difference ((dag dag) (other dag))
  (let ((result (make-instance 'dag)))
    (dolist (commit (all-commits dag))
      (unless (get-commit other (sha commit))
        (add-commit result commit)))
    result))

(defun listify (x)
  (if (listp x)
      x
      (list x)))

(defmethod reachable-nodes ((dag dag) commits &key (depth 1000)
                                                (seen-callback #'identity)
                                                excludes)
  "Find all the reachable nodes from a specific commit, upto the given DEPTH. Does not return partial nodes (i.e. commits whose information we don't have).

COMMITS can either be a single commit or a list of commits.

If SEEN-CALLBACK is provided, then that is called with the commit each
time we see a new node. This will include partial nodes.

EXCLUDES is an optional list of SHAs. If provided, we don't find reachable nodes that require a
path via any SHA in EXCLUDES.
"
  (let ((commits (listify commits))
        (depths (make-hash-table :test #'equal))
        (sha-seen (make-hash-table :test #'equal))
        (queue (make-queue)))

    (loop for sha in excludes
          do (setf (gethash sha sha-seen) t))

    
    (dolist (commit commits)
      (enqueue commit queue)
      (setf (gethash commit depths) 1))

    (macrolet ((sha-seen (hash)
                 `(gethash ,hash sha-seen)))
      (dolist (commit commits)
        (setf (sha-seen commit) t))

      (loop while (not (queue-emptyp queue))
            for sha = (dequeue queue)
           do
              (let ((commit (get-commit dag sha))
                    (this-depth (gethash sha depths)))
                (cond
                  (commit
                   (funcall seen-callback commit)
                   (when (< this-depth depth)
                     (loop for parent in (parents commit) do
                       (unless (gethash parent depths)
                         (setf (gethash parent depths) (1+ this-depth)))
                       (unless (sha-seen parent)
                         (setf (sha-seen parent) t)
                         (enqueue parent queue)))))
                  (t
                   ;; This node isn't present in the graph, but we
                   ;; still want to indicate that we've seen it.
                   (funcall seen-callback
                            (make-instance 'commit :sha sha)))))))
    (loop for sha in excludes
          do (remhash sha sha-seen))
    (loop for sha being the hash-keys of sha-seen
          for node = (get-commit dag sha)
          if node
            collect node)))

(defun hash-table-intersection (ht1 ht2)
  (let ((result (make-hash-table)))
   (loop for k being the hash-keys of ht1
         if (gethash k ht2)
           do (setf (gethash k result) t))
    result))

(defun list-to-hash-table (list)
  (let ((hash-table (make-hash-table)))
    (loop for elem in list
          do (setf (gethash elem hash-table) t))
    hash-table))

(defmethod merge-bases-for-depth ((dag dag) commit-1s commit-2
                                  &key
                                    depth
                                    exclude-commit-2)
  "See https://stackoverflow.com/questions/14865081/algorithm-to-find-lowest-common-ancestor-in-directed-acyclic-graph

However, here's a better description. Find all the ancestors, this
forms a subgraph. Now find all the source nodes of that subgraph."
  (with-extras (("commit-1" commit-1s)
                ("commit-2" commit-2))
   (let* ((excludes (when exclude-commit-2
                      (list commit-2)))
          (reachable-1 (list-to-hash-table (reachable-nodes dag commit-1s :depth depth
                                                                          :excludes excludes)))
          (reachable-2 (list-to-hash-table (reachable-nodes dag commit-2 :depth depth))))
     (let ((intersection (hash-table-intersection reachable-1 reachable-2))
           (definitely-not-source-nodes (make-hash-table :test #'equal)))
       (loop for commit being the hash-keys of intersection
             do
                (dolist (parent (parents commit))
                  (setf (gethash parent definitely-not-source-nodes) t)))

       (let ((merge-bases (loop for commit being the hash-keys of intersection
                                unless (gethash (sha commit) definitely-not-source-nodes)
                                  collect (sha commit))))
         merge-bases)))))

(defmethod merge-base ((dag dag) commit-1 commit-2
                       &key (exclude-commit-2 t))
  "Find the merge-base for merging commit-2 into the branch indicated
with commit-1. Note that the merge-base might not be symmetrical. If
there are multiple equivalent merge-bases, then one will be returned
arbitrarily.

COMMIT-1 can either be a string or a list of strings. If it's a list,
then it's equivalent to a new commit that has all those commits as
parents. If COMMIT-1 is empty, then we always return NIL.

If EXCLUDE-COMMIT-2 is true, then we'll exclude any node that's only
an ancestor of COMMIT-1 via COMMIT-2. This is useful for pull-requests
that might've already merged."
  (let ((commit-1 (listify commit-1)))
   (cond
     ((equal commit-1 (list commit-2))
      ;; Technically this is incorrect when EXCLUDE-COMMIT-2 is provided,
      ;; but abstract-pr-promoter depends on this behavior.
      commit-2)
     (t
      (let* ((merge-bases
               (flet ((try-depth (depth)
                        (merge-bases-for-depth dag commit-1 commit-2
                                               :depth depth
                                               :exclude-commit-2 exclude-commit-2)))
                 (or
                  ;; Micro optimization: First look only at the last 100 nodes.
                  (try-depth 100)
                  (try-depth 1000)
                  ;; This likely means that all paths from commit-1 go
                  ;; through commit-2.
                  (loop for commit-1 in commit-1
                   if (ancestorp dag commit-2 commit-1)
                       return (list commit-2))))))
        (let ((merge-bases (sort (copy-list merge-bases)
                                 #'>
                                 :key (lambda (sha)
                                        (commit-timestamp (get-commit dag sha))))))
          (when (> (length merge-bases) 1)
            (with-extras (("result-merge-bases" merge-bases))
             (warn "fun warning! we hit multiple merge-bases in production!")))
          (values (car merge-bases)
                  merge-bases)))))))

(defmethod best-path ((dag dag) (sha-1 string) (sha-2 string) &key (max-depth 100))
  "Find the best path from SHA-1 to SHA-2, where SHA-2 is the ancestor.

Given two paths, x0->x1->....->xn and y0->y1->...>yn, find the first
position `i` (0<i<n) where they differ. Look at the edges from
x_{i-1}->x_i and y{i-1}->y_i. Whichever one is the leftmost parent,
that path is the better path.

This approximately translates to trying to find a path between the
commits using leftmost parents when possible.

By this definition, the best solution is to use a DFS. (An alternate
definition is to give a high cost to the non-leftmost
parents. However, it adds more complexity, and does not guarantee a
unique solution.)

We don't attempt to find a path if the path is longer than
MAX-DEPTH. This makes it acceptable to use recursion for now.
"
  (let ((seen (make-hash-table :test #'equal)))
    (labels ((dfs (sha max-depth path)
               (let ((parents (?. parents (get-commit dag sha))))
                 (cond
                   ((gethash sha seen)
                    nil)
                   ((<= max-depth 0)
                    ;; could not find a path
                    nil)
                   ((equal sha sha-2)
                    (list* sha path))
                   (t
                    (setf (gethash sha seen) t)
                    (loop for parent in parents
                          for solved-path = (dfs parent (1- max-depth) (list* sha path))
                          if solved-path
                            return solved-path)))))
             )
      (reverse
       (dfs sha-1 max-depth nil)))))
