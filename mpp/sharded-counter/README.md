# Sharded Counter

Making a counter scalable is non-trivial, as a simple solution
with a single atomic register updated via `Fetch-and-Add` induces
high contention. However, it is possible to make the counter updates
more efficient by making the implementation relaxed.
At the high level, the counter is split into multiple shards, so
increments update random registers (all of them should be placed on
differed cache lines), while reads summarize all the shards.

Please implement the `inc()` and `get()` operations in `ShardedCounter`,
and prove that it is linearizable. Please also find a non-linearizable
execution when `inc(delta)` operation is provided.

To test your solution, please run:
* `./gradlew build` on Linux or MacOS
* `gradlew build` on Windows