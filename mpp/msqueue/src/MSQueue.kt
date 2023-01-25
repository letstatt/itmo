package mpp.msqueue

import kotlinx.atomicfu.AtomicRef
import kotlinx.atomicfu.atomic

class MSQueue<E> {
    private val head: AtomicRef<Node<E>>
    private val tail: AtomicRef<Node<E>>

    init {
        val dummy = Node<E>(null)
        head = atomic(dummy)
        tail = atomic(dummy)
    }

    /**
     * Adds the specified element [x] to the queue.
     */
    fun enqueue(x: E) {
        val node = Node(x)

        while (true) {
            val last = tail.value
            if (last.next.compareAndSet(null, node)) {
                tail.compareAndSet(last, node)
                // if last CAS failed, someone helped us
                break

            } else {
                // help and repeat
                last.next.value?.let { tail.compareAndSet(last, it) }
            }
        }
    }

    /**
     * Retrieves the first element from the queue
     * and returns it; returns `null` if the queue
     * is empty.
     */
    fun dequeue(): E? {
        while (true) {
            val first = head.value
            val last = tail.value

            if (first == last) {
                if (last.next.value == null) {
                    return null
                } else {
                    // help and repeat
                    last.next.value?.let { tail.compareAndSet(last, it) }
                }
            } else {
                val next = first.next.value
                if (next?.let { head.compareAndSet(first, it) } == true) {
                    return next.x
                }
            }
        }
    }

    fun isEmpty(): Boolean {
        while (true) {
            val first = head.value
            val last = tail.value

            if (first == last) {
                if (last.next.value == null) {
                    return true
                } else {
                    // help and repeat
                    last.next.value?.let { tail.compareAndSet(last, it) }
                }
            } else {
                return false
            }
        }
    }
}

private class Node<E>(val x: E?) {
    val next = atomic<Node<E>?>(null)
}