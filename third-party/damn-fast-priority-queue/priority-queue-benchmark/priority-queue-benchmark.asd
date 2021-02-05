;;;; priority-queue-benchmark.asd

(asdf:defsystem #:priority-queue-benchmark
  :description "Figure out the fastest priority queue implementation yourself."
  :author "Michał \"phoe\" Herda <phoe@disroot.org>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on
  (;; Benchmark dependencies
   #:alexandria #:trivial-garbage
   ;; Contestants
   #:pettomato-indexed-priority-queue #:priority-queue #:queues.priority-queue
   #:pileup #:bodge-heap #:cl-heap #:heap #:minheap
   #:damn-fast-priority-queue #:damn-fast-stable-priority-queue)
  :components ((:file "benchmark"))
  :perform (test-op (o c) (symbol-call "PRIORITY-QUEUE-BENCHMARK" "RUN")))
