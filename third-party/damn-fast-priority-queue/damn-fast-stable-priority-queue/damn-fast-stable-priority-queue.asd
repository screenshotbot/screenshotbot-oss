;;;; damn-fast-stable-priority-queue.asd

(asdf:defsystem #:damn-fast-stable-priority-queue
  :description "A heap-based stable priority queue whose first and foremost priority is speed."
  :author "Michał \"phoe\" Herda <phoe@disroot.org>"
  :license  "MIT"
  :version "0.0.2"
  :serial t
  :depends-on (#:alexandria)
  :components ((:file "src"))
  :in-order-to ((test-op (load-op #:damn-fast-stable-priority-queue/test)))
  :perform (test-op (o c) (symbol-call "DAMN-FAST-STABLE-PRIORITY-QUEUE/TEST" "RUN")))

(asdf:defsystem #:damn-fast-stable-priority-queue/test
  :description "Tests for Damn Fast Stable Priority Queue"
  :author "Michał \"phoe\" Herda <phoe@disroot.org>"
  :license "MIT"
  :version "0.0.2"
  :serial t
  :depends-on (#:alexandria #:damn-fast-stable-priority-queue)
  :components ((:file "test-distinct")
               (:file "test-same")
               (:file "test")))
