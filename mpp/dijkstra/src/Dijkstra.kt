package dijkstra

import java.util.*
import java.util.concurrent.Phaser
import java.util.concurrent.ThreadLocalRandom
import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.atomic.AtomicInteger
import kotlin.Comparator
import kotlin.concurrent.thread

private val NODE_DISTANCE_COMPARATOR = Comparator<Node> { o1, o2 -> Integer.compare(o1!!.distance, o2!!.distance) }

private class PriorityMultiQueue(workers: Int) {
    private var queues : Array<PriorityQueue<Node>?>
    private var locks : Array<AtomicBoolean?>
    private val multiplier = 5
    private var size : Int

    init {
        assert(workers > 0)
        size = multiplier * workers
        queues = arrayOfNulls(size)
        locks = arrayOfNulls(size)

        for (i in locks.indices) {
            locks[i] = AtomicBoolean(false)
        }

        for (i in queues.indices) {
            queues[i] = PriorityQueue(50, NODE_DISTANCE_COMPARATOR)
        }
    }

    fun insert(task: Node) {
        while (true) {
            val ind = ThreadLocalRandom.current().nextInt(size)
            val queue = queues[ind]!!
            val lock = locks[ind]!!

            if (!lock.compareAndSet(false, true)) {
                // queue is locked, try again
                continue
            }
            queue.add(task)
            lock.compareAndSet(true, false)
            break
        }
    }

    fun delete() : Node? {
        while (true) {
            val rnd = ThreadLocalRandom.current()
            val i1 = rnd.nextInt(size)
            val i2 = rnd.nextInt(size)
            val t1 = queues[i1]!!.peek()
            val t2 = queues[i2]!!.peek()

            if (t1 == null && t2 == null) {
                return null
            }

            val ind = if (t1 == null) {
                i2
            } else if (t2 == null) {
                i1
            } else {
                if (NODE_DISTANCE_COMPARATOR.compare(t1, t2) < 0) i1 else i2
            }

            if (!locks[ind]!!.compareAndSet(false, true)) {
                // queue is locked, try again
                continue
            }


            val queue = queues[ind]!!
            val t = queue.poll()

            locks[ind]!!.compareAndSet(true, false)
            return t
        }
    }
}

// Returns `Integer.MAX_VALUE` if a path has not been found.
fun shortestPathParallel(start: Node) {
    val workers = Runtime.getRuntime().availableProcessors()
    // The distance to the start node is `0`
    start.distance = 0
    // Create a priority (by distance) queue and add the start node into it
    val q = PriorityMultiQueue(workers)
    q.insert(start)
    // Run worker threads and wait until the total work is done
    val onFinish = Phaser(workers + 1) // `arrive()` should be invoked at the end by each worker
    val jobs = AtomicInteger()
    jobs.incrementAndGet()
    repeat(workers) {
        thread {
            while (jobs.get() > 0) {
                val cur: Node = q.delete() ?: continue
                for (e in cur.outgoingEdges) {
                    while (true) {
                        val old = e.to.distance
                        val new = cur.distance + e.weight
                        if (old > new) {
                            if (e.to.casDistance(old, new)) {
                                q.insert(e.to)
                                jobs.incrementAndGet()
                                break
                            } else {
                                continue
                            }
                        } else {
                            break
                        }
                    }
                }
                jobs.decrementAndGet()
            }
            onFinish.arrive()
        }
    }
    onFinish.arriveAndAwaitAdvance()
}