package mpp.faaqueue

import kotlinx.atomicfu.*

class FAAQueue<E> {
    private val head: AtomicRef<Segment> // Head pointer, similarly to the Michael-Scott queue (but the first node is _not_ sentinel)
    private val tail: AtomicRef<Segment> // Tail pointer, similarly to the Michael-Scott queue
    private val tombstone = Tombstone()
    private class Tombstone

    init {
        val firstNode = Segment()
        head = atomic(firstNode)
        tail = atomic(firstNode)
    }

    /**
     * Adds the specified element [x] to the queue.
     */
    fun enqueue(element: E) {
        while (true) {
            val curTail = tail.value
            val next = curTail.next.value

            if (next != null) {
                tail.compareAndSet(curTail, next)
                continue // try again
            }

            val i = curTail.enqIdx.getAndIncrement()

            if (i >= SEGMENT_SIZE) { // out of segment, try to add a new one
                val nextSegment = Segment()
                nextSegment.put(0, element)
                if (curTail.next.compareAndSet(null, nextSegment)) {
                    return // success
                }
            } else if (curTail.cas(i.toInt(), null, element)) {
                return // success
            }
        }
    }

    /**
     * Retrieves the first element from the queue and returns it;
     * returns `null` if the queue is empty.
     */
    fun dequeue(): E? {
        while (true) {
            val curHead = head.value
            val i = curHead.deqIdx.getAndIncrement()

            if (/*i >= curHead.enqIdx.value || */i >= SEGMENT_SIZE) {
                val nextHead = curHead.next.value ?: return null // queue is empty
                head.compareAndSet(curHead, nextHead) // roll again

            } else {
                if (curHead.cas(i.toInt(), null, tombstone)) {
                    continue // enqueue stuck, roll again
                } else {
                    return curHead.get(i.toInt()) as E? // success
                }
            }
        }
    }

    /**
     * Returns `true` if this queue is empty, or `false` otherwise.
     */
    val isEmpty: Boolean
        get() {
            while (true) {
                val curHead = head.value
                val i = curHead.deqIdx.value
                if (i >= curHead.enqIdx.value || i >= SEGMENT_SIZE) {
                    val nextHead = curHead.next.value ?: return true // queue is empty
                    head.compareAndSet(curHead, nextHead) // roll again
                } else {
                    return false
                }
            }
        }
}

private class Segment {
    val next: AtomicRef<Segment?> = atomic(null)
    val elements = atomicArrayOfNulls<Any>(SEGMENT_SIZE)
    val enqIdx = atomic(0L)
    val deqIdx = atomic(0L)

    fun get(i: Int) = elements[i].value
    fun cas(i: Int, expect: Any?, update: Any?) = elements[i].compareAndSet(expect, update)
    fun put(i: Int, value: Any?) {
        elements[i].value = value
    }
}

const val SEGMENT_SIZE = 2 // DO NOT CHANGE, IMPORTANT FOR TESTS

