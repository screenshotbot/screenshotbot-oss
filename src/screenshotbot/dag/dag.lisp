;; Copyright 2018-Present Modern Interpreters Inc.
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(in-package :dag)

(define-condition node-already-exists (error)
  ())

(defvar *magic* (flex:string-to-octets "bdag" :external-format :latin1))

(defclass commit ()
  ((sha :initarg :sha
        :accessor sha)
   (author :initarg :author
           :initform nil
           :accessor author)
   (parents :initarg :parents
            :initform nil
            :accessor parents
            :documentation "array of shas")))

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

(defclass dag ()
  ((commits :initform (make-hash-table :test 'equal)
            :initarg :commit-map
            :accessor commit-map
            :documentation "A map from COMMIT-NODE-ID (number) to COMMIT. We eventually plan to replace this with sha to COMMIT.")
   (pathname :initarg :pathname
             :documentation "For debugging only")))

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

(defmethod safe-topological-sort (dag)
  "This is modified from GRAPH. Sadly that library uses recursion for
some of their graph algorithms, which doesn't work nicely for a 'deep'
tree. This version uses the Kahn's algorithm instead of DFS"
  ;;(declare (optimize (speed 3) (debug 0)))
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
  (let ((dag (make-instance 'dag :pathname (or
                                            (ignore-errors
                                             (pathname stream))
                                            "Stream not pointing to file"))))
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

(defmethod reachable-nodes ((dag dag) commit &key (depth 1000)
                                               (seen-callback #'identity))
  "Find all the reachable nodes from a specific commit, upto the given DEPTH. Does not return partial nodes (i.e. commits whose information we don't have).

If SEEN-CALLBACK is provided, then that is called with the commit each
time we see a new node. This will include partial nodes.
"
  (let ((depths (make-hash-table :test #'equal))
        (sha-seen (make-hash-table :test #'equal))
        (queue (make-queue)))

    (enqueue commit queue)
    (setf (gethash commit depths) 1)

    (macrolet ((sha-seen (hash)
                 `(gethash ,hash sha-seen)))
      (setf (sha-seen commit) t)

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

(defmethod merge-base-for-depth ((dag dag) commit-1 commit-2 &key depth)
  (let ((reachable-1 (list-to-hash-table (reachable-nodes dag commit-1 :depth depth)))
        (reachable-2 (list-to-hash-table (reachable-nodes dag commit-2 :depth depth))))
    (let ((intersection (hash-table-intersection reachable-1 reachable-2)))
      ;; Now that we found an intersection, we just need to find the
      ;; first reachable node in this set (from either commit-1 or
      ;; commit-2, it doesn't matter)

      (reachable-nodes dag commit-2
                       :depth depth
                       :seen-callback (lambda (commit)
                                        (when (gethash commit intersection)
                                          (return-from merge-base-for-depth (sha commit)))))))
  nil)

(defmethod merge-base ((dag dag) commit-1 commit-2)
  (or
   ;; Micro optimization: First look only at the last 100 nodes.
   (merge-base-for-depth dag commit-1 commit-2 :depth 100)
   (merge-base-for-depth dag commit-1 commit-2 :depth 1000)))
