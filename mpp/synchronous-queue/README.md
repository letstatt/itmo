# Synchronous Queue
You will implement a synchronous queue algorithm for Kotlin Coroutines in this task.
You are up to implementing either the algorithm based on the Michael-Scott queue
(see [this paper](https://www.cs.rochester.edu/u/scott/papers/2009_Scherer_CACM_SSQ.pdf))
or the `FAA`-based one discussed in class.

The `SynchronousQueueSequential` class in
 [`SynchronousQueueTest.kt`](test/SynchronousQueueTest.kt) illustrates how to manage coroutines. 
You may need to restart the operation after a `suspendCoroutine` call, the code below does the trick:

```kotlin 
val res = suspendCoroutine<Any> sc@ { cont ->
  ...
  if (shouldRetry()) {
    cont.resume(RETRY)
    return@sc
  }
  ...
}
if (res === RETRY) continue
```                

To test your solution, please run:

* `./gradlew build` on Linux or MacOS
* `gradlew build` on WindowsА 