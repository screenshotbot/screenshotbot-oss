# Damn Fast Priority Queue

A heap-based priority queue whose first and foremost priority is [**speed**](https://www.youtube.com/watch?v=AkagvXwDsYU). Optionally comes in a stable flavor.

Blame [@mfiano](https://github.com/mfiano/) for the existence of this library. He's the one who wanted a priority queue that's going to run as fast as possible in one of the hottest loops of his game engine ~~and then figured out that hey, he actually doesn't need a prio queue there~~.

## License

MIT.

## Systems

* This repository contains two systems:
  * `damn-fast-priority-queue`, a faster but unstable priority queue (elements with the same priority are dequeued in unspecified order),
  * `damn-fast-stable-priority-queue`, a fast and stable priority queue (elements with the same priority are dequeued in FIFO order).
* The queues have identical APIs.
  * These APIs are not generic, i.e. operators for one queue type must not be used on a queue instance of the other type.

## Description

* The queue enqueues objects along with their priorities.
  * The stored objects may be of arbitrary type.
  * The objects' priorities must be of type `(unsigned-byte 32)`.
* The queue is a minimum queue (i.e. smallest priorities are dequeued first).
* The queue is unbounded by default.
  * The queue's storage automatically expands (which reallocates the queue storage).
  * The queue's storage can be manually trimmed (which reallocates the queue storage).
  * The queue can instead be configured to signal an error upon reaching its size limit.
* The queue is **not** thread-safe.
* The queue is **not** reentrant.

## Implementation details

* The queue internally uses two simple vectors: one for data, specialized on `t`, and another for priorities, specialized on `(unsigned-byte 32)`.
  * The stable queue also uses a third simple vector for storing element insertion order, specialized on `(unsigned-byte 32)`.
* The queue's storage has its initial storage size set to `256`. This value is customizable in the constructor.
* Each time the queue runs out of storage, the storage is reallocated via `adjust-array` and its size is expanded by the `extension-factor` value provided at queue instantiation.
* We assume that using simple vectors, calling `adjust-array` on them, and manually setting queue slots to the new vectors is faster than using adjustable vectors.

## Optimization settings

* The code uses structure classes in favor of standard classes.
* The code uses standard, `inline`-proclaimed functions in favor of generic functions.
* All functions are optimized for maximum `speed`.
* By default, the code retains the default values of `debug`, `safety`, `space`, and `compilation-speed` optimize qualities. To set them all to 0, pray to your favorite deity and push a feature into `*features*` before compiling the respective system.
  * for `damn-fast-priority-queue`, push `:real-damn-fast-priority-queue`,
  * for `damn-fast-stable-priority-queue`, push `:real-damn-fast-stable-priority-queue`.

## Exports

All exported functions are proclaimed `inline` by default.

* **Classes**
  * `queue` - names the priority queue structure class.
* **Functions**
  * `(make-queue &optional initial-storage-size extension-factor extend-queue-p)` - make a priority queue.
    * The initial storage size must be a non-negative integer. Its default value is `256`.
    * The extension factor value must be a positive integer between `2` and `256`. Its default value is `2`.
    * The queue can be configured to signal an error of type `queue-size-limit-reached` when its size is reached, instead of extending its storage. It is possible to retrieve the queue via the `queue-size-limit-reached-queue` reader and the object that was attempted to be stored via `queue-size-limit-reached-object`.
  * `(copy-queue queue)` - makes a deep copy of the provided queue (including its storage vectors).
  * `(enqueue queue object priority)` - enqueue an object.
  * `(dequeue queue)` - dequeue an object.
    * Secondary return value is true if the object was found and false if the queue was empty.
  * `(peek queue)` - peek at an object that is first to be dequeued.
    * Secondary return value is true if the object was found and false if the queue was empty.
  * `(size queue)` - get the current element count of the queue.
  * `(trim queue)` - trim the queue's storage by calling `adjust-array` on it with the current queue size.
  * `(map queue function)` - calls the function on each element of `queue` in unspecified order and returns `nil`.
* **Macros**
  * `(do-queue (object queue &optional result) &body body)` - evaluates `body` with `object` bound to each element of `queue` in unspecified order and returns `result`.

## Tests

* For `damn-fast-priority-queue`:
  * Non-verbose test: `(asdf:test-system :damn-fast-priority-queue)`
  * Verbose test: `(damn-fast-priority-queue/test:run t)`
* For `damn-fast-stable-priority-queue`:
  * Non-verbose test: `(asdf:test-system :damn-fast-stable-priority-queue)`
  * Verbose test: `(damn-fast-stable-priority-queue/test:run t)`

## Performance tests

See [the Priority Queue Benchmark README](priority-queue-benchmark/README.md).
