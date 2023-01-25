# Multi-Word CAS with the distinct values assumptions

In this task, you need to implement the atomic `cas2(..)` for `AtomicArray`,
which atomically changes two locations in the array. Please use the multi-word
CAS algorithm discussed in class. Remember to always install k-CAS descriptors
in the same order.

**In this task, you may assume that all values are distinct, 
which eliminates the ABA problem and the necessity to use 
DCSS descriptors.**

To test your solution, please run:

* `./gradlew build` on Linux or MacOS
* `gradlew build` on Windows