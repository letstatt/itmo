# Dynamic Array
In this task, you will implement a lock-free dynamic array
(a.k.a. `vector` in C++ and `ArrayList` in Java).
Please use the ideas behind the open addressing hash table 
for the resize procedure. Please be careful with the `pushBack(..)`
operation -- it must be atomic.

You do not need to implement an efficient cooperative elements 
transition; each thread is eligible to go over the whole array 
to guarantee that all elements are successfully moved to a new 
version of the array.



To test your solution, please run:

* `./gradlew build` on Linux or MacOS
* `gradlew build` on Windows