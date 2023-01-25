# Flat-Combining Priority Queue

Implementing fast and scalable priority queue is non-trivial. 
One of the most efficient technique that helps to construct 
a linearizable implementation is *flat combining*, which can be
used with the fast sequential binary heap under the hood.

In this task, you need to implement such a priority queue in [FCPriorityQueue](src/FCPriorityQueue.kt).

Related papers:

* [Flat combining and the synchronization-parallelism tradeoff](https://dl.acm.org/doi/pdf/10.1145/1810479.1810540?casa_token=Yo13gxOeFhwAAAAA:qS33gvUFNhI4t_2ioHnZz0egK8PFq0Mg7MT0ma1-26aeQYKk7aZBzEHEY6iFMiu-GEmzsBMuSibDkg)

To test your solution, please run:

* `./gradlew build` on Linux or MacOS
* `gradlew build` on Windows