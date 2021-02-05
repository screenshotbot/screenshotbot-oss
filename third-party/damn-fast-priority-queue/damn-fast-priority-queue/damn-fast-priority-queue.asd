;;;; damn-fast-priority-queue.asd

(asdf:defsystem #:damn-fast-priority-queue
  :description "A heap-based priority queue whose first and foremost priority is speed."
  :author "Michał \"phoe\" Herda <phoe@disroot.org>"
  :license  "MIT"
  :version "0.0.2"
  :serial t
  :depends-on (#:alexandria)
  :components ((:file "src"))
  :in-order-to ((test-op (load-op #:damn-fast-priority-queue/test)))
  :perform (test-op (o c) (symbol-call "DAMN-FAST-PRIORITY-QUEUE/TEST" "RUN")))

(asdf:defsystem #:damn-fast-priority-queue/test
  :description "Tests for Damn Fast Priority Queue"
  :author "Michał \"phoe\" Herda <phoe@disroot.org>"
  :license "MIT"
  :version "0.0.2"
  :serial t
  :depends-on (#:alexandria #:damn-fast-priority-queue)
  :components ((:file "test")))
