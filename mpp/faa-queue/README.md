# FAA-based queue

In this task, you will implement a concurrent queue that leverages the `Fetch-and-Add` synchronization primitive. 
The high-level design of this queue bases on a conceptually infinite array for storing elements and manipulates 
`enqIdx` and `deqIdx` counters, which reference the next working cells in the infinite array for `enqueue(..)` 
and `dequeue()` operations. The infinite array implementation is usually simulated via a linked list of 
fixed-size segments. The overall algorithm should be at least obstruction-free.

Related papers:
* [Fast Concurrent Queues for x86 Processors](https://www.cs.tau.ac.il/~mad/publications/ppopp2013-x86queues.pdf)
* [A Wait-free Queue as Fast as Fetch-and-Add](http://chaoran.me/assets/pdf/wfq-ppopp16.pdf)

To test your solution, please run:

* `./gradlew build` on Linux or MacOS
* `gradlew build` on Windows