# Priority Queue Benchmark

The ASDF system `priority-queue-benchmark` contains a simple performance test with three tweakable parameters:
* `+capacity+` - how many elements will be pushed into the queue,
* `+repeat-count+` - how many times the test will be repeated,
* `+pass-capacity-p+` - should the test pass the value of `+capacity+` into the queue?
  * Note: not all tested queue libraries support passing the initial capacity or extension factor as parameters when constructing the queue. Therefore, for some libraries, this parameter is a no-op.

The performance test includes multiple priority queue/heap libraries available on Quicklisp, tested against four synthetic datasets:
* an array of unique numbers from `0` to `n`, in increasing order,
* an array of unique numbers from `0` to `n`, in decreasing order,
* an array of unique numbers from `0` to `n`, in shuffled order,
* an array of `n` zeroes.

All test functions are compiled with `(optimize speed)`.

`:real-damn-fast-priority-queue` is `:damn-fast-priority-queue` compiled with `:real-damn-fast-priority-queue` pushed into `*features*`.

`:real-damn-fast-stable-priority-queue` is `:damn-fast-stable-priority-queue` compiled with `:real-damn-fast-stable-priority-queue` pushed into `*features*`.

The listed timing does not include the time required to prepare the test vectors or to construct the priority queue object.

Please feel free to question, verify, and improve the code and results of this benchmark.

## 409600 elements, 10 repeats, capacity passed

| Library \ Vector                      | :increasing | :decreasing | :shuffled |     :zero |
|---------------------------------------|-------------|-------------|-----------|-----------|
| :pettomato-indexed-priority-queue     |       4.323 |       5.943 |     6.463 |     0.687 |
| :priority-queue                       |       6.335 |      10.195 |     7.251 |     0.911 |
| :queues.priority-queue                |       7.663 |       7.295 |    13.539 |     3.463 |
| :pileup                               |       2.019 |       2.207 |     2.143 |     2.083 |
| :bodge-heap                           |       5.703 |      12.203 |     6.359 |     5.039 |
| :cl-heap                              |       9.483 |      10.471 |    28.119 |    29.563 |
| :heap                                 |       9.491 |      12.167 |     9.287 |     0.895 |
| :minheap                              |       6.335 |       7.951 |     7.663 | **0.551** |
| :damn-fast-priority-queue             |   **0.599** |   **0.719** | **0.819** |     0.631 |
| :real-damn-fast-priority-queue        |   **0.543** |   **0.647** | **0.739** |     0.599 |
| :damn-fast-stable-priority-queue      |       0.663 |       0.807 |     1.031 |     0.791 |
| :real-damn-fast-stable-priority-queue |       0.543 |       0.723 |     0.899 |     0.599 |

## 409600 elements, 10 repeats, capacity not passed

| Library \ Vector                      | :increasing | :decreasing | :shuffled |     :zero |
|---------------------------------------|-------------|-------------|-----------|-----------|
| :pettomato-indexed-priority-queue     |       4.759 |       5.963 |     7.311 |     0.671 |
| :priority-queue                       |       6.339 |      10.067 |     7.427 |     0.915 |
| :queues.priority-queue                |       7.223 |       7.207 |    11.807 |     3.487 |
| :pileup                               |       1.851 |       2.187 |     2.111 |     2.103 |
| :bodge-heap                           |       5.423 |      12.403 |     8.051 |     5.067 |
| :cl-heap                              |       9.035 |      10.715 |    31.211 |    28.767 |
| :heap                                 |       8.583 |      12.263 |     8.999 |     0.875 |
| :minheap                              |       5.523 |       7.789 |    11.335 | **0.535** |
| :damn-fast-priority-queue             |   **0.575** |   **0.727** | **0.931** |     0.627 |
| :real-damn-fast-priority-queue        |   **0.519** |   **0.623** | **0.715** |     0.599 |
| :damn-fast-stable-priority-queue      |       0.635 |       0.803 |     1.007 |     0.795 |
| :real-damn-fast-stable-priority-queue |       0.555 |       0.727 |     0.887 |     0.639 |

## 4096 elements, 1000 repeats, capacity passed

| Library \ Vector                      | :increasing | :decreasing | :shuffled |     :zero |
|---------------------------------------|-------------|-------------|-----------|-----------|
| :pettomato-indexed-priority-queue     |       2.567 |       3.599 |     2.887 |     0.675 |
| :priority-queue                       |       3.863 |       5.995 |     4.359 |     0.863 |
| :queues.priority-queue                |       5.491 |       4.963 |     5.231 |     2.583 |
| :pileup                               |       1.391 |       1.591 |     1.471 |     1.487 |
| :bodge-heap                           |       3.471 |       8.271 |     3.991 |     3.127 |
| :cl-heap                              |       6.467 |      10.795 |    13.807 |    13.807 |
| :heap                                 |       5.187 |       8.339 |     5.587 |     0.879 |
| :minheap                              |       3.135 |       5.767 |     3.531 |     0.527 |
| :damn-fast-priority-queue             |   **0.375** |   **0.455** | **0.471** | **0.267** |
| :real-damn-fast-priority-queue        |   **0.327** |   **0.371** | **0.411** | **0.215** |
| :damn-fast-stable-priority-queue      |       0.435 |       0.491 |     0.511 |     0.491 |
| :real-damn-fast-stable-priority-queue |       0.351 |       0.419 |     0.435 |     0.419 |

## 4096 elements, 1000 repeats, capacity not passed

| Library \ Vector                      | :increasing | :decreasing | :shuffled |     :zero |
|---------------------------------------|-------------|-------------|-----------|-----------|
| :pettomato-indexed-priority-queue     |       2.555 |       3.607 |     2.943 |     0.699 |
| :priority-queue                       |       3.831 |       6.647 |     4.311 |     0.867 |
| :queues.priority-queue                |       4.855 |       6.579 |     5.231 |     2.567 |
| :pileup                               |       1.399 |       1.611 |     1.503 |     1.479 |
| :bodge-heap                           |       3.419 |       8.199 |     4.171 |     3.107 |
| :cl-heap                              |       6.479 |       8.527 |    13.967 |    13.987 |
| :heap                                 |       5.243 |       7.539 |     5.619 |     0.915 |
| :minheap                              |       3.215 |       4.487 |     3.471 |     0.547 |
| :damn-fast-priority-queue             |   **0.379** |   **0.467** | **0.467** | **0.271** |
| :real-damn-fast-priority-queue        |   **0.323** |   **0.387** | **0.415** | **0.215** |
| :damn-fast-stable-priority-queue      |       0.403 |       0.491 |     0.495 |     0.491 |
| :real-damn-fast-stable-priority-queue |       0.347 |       0.427 |     0.431 |     0.411 |
