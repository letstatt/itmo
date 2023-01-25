package mpp.skiplist

import java.util.concurrent.ThreadLocalRandom
import java.util.concurrent.atomic.AtomicMarkableReference

// lock-free implementation from "The Art of Multiprocessor Programming"

class SkipList<E : Comparable<E>> {
    private val head = Node<E>(null, MAX_LEVEL)
    private val tail = Node<E>(null, MAX_LEVEL)

    init {
        for (i in 0 until head.next.size) {
            head.next[i].set(tail, false)
        }
    }

    private fun finder(element: E): Result<E> {
        val bottomLevel = 0

        retry@ while(true) {
                var prev = head
                var cur: Node<E> = prev
                var next: Node<E>
                val result = Result<E>()

                for (i in MAX_LEVEL-1 downTo bottomLevel) {
                    cur = prev.next[i].reference

                    while (cur != tail) {
                        val marked = BooleanArray(1)
                        next = cur.next[i].get(marked)
                        while (marked[0]) { // tail is invulnerable
                            next = cur.next[i].get(marked)
                            val snip = prev.next[i].compareAndSet(cur, next, false, false)
                            if (!snip) {
                                continue@retry
                            }
                            cur = prev.next[i].reference
                        }
                        if (cur != tail && cur.element!! < element) {
                            prev = cur
                            cur = next
                        } else {
                            break
                        }
                    }
                    result.prev[i] = prev
                    result.next[i] = cur
                }
                if (cur != tail && cur.element!! == element) {
                    result.found = true
                }
                return result
            }
    }

    /**
     * Adds the specified element to this set
     * if it is not already present.
     *
     * Returns `true` if this set did not
     * already contain the specified element.
     */
    fun add(element: E): Boolean {
        val node = Node(element)

        while (true) {
            var result = finder(element)
            if (result.found) {
                return false
            } else {
                for (i in 0 until node.levels) {
                    node.next[i] = AtomicMarkableReference(result.next[i], false)
                }
                var prev = result.prev[0]!!
                var next = result.next[0]!!
                if (!prev.next[0].compareAndSet(next, node, false, false)) {
                    continue // very first set failed, retry
                }
                for (i in 1 until prev.levels.coerceAtMost(node.levels)) {
                    while (true) {
                        prev = result.prev[i]!!
                        next = result.next[i]!!
                        if (prev.next[i].compareAndSet(next, node, false, false)) {
                            break // good, go ahead
                        }
                        result = finder(element) // prev/next changed, retry
                    }
                }
                return true
            }
        }
    }

    /**
     * Removes the specified element from this set
     * if it is present.
     *
     * Returns `true` if this set contained
     * the specified element.
     */
    fun remove(element: E): Boolean {
        while (true) {
            val result = finder(element)
            if (!result.found) {
                return false
            } else {
                val nodeToRemove = result.next[0]!!
                for (i in nodeToRemove.levels-1 downTo 1) {
                    val marked = BooleanArray(1)
                    var next = nodeToRemove.next[i].get(marked)
                    while (!marked[0]) {
                        nodeToRemove.next[i].attemptMark(next, true)
                        next = nodeToRemove.next[i].get(marked)
                    }
                }
                // only bottom level not marked now
                val marked = BooleanArray(1)
                var next = nodeToRemove.next[0].get(marked)
                while (true) {
                    val iMarkedIt = nodeToRemove.next[0].attemptMark(next, true)
                    next = nodeToRemove.next[0].get(marked)
                    if (iMarkedIt) {
                        finder(element) // remove physically
                        return true
                    } else if (marked[0]) {
                        return false
                    }
                }
            }
        }
    }

    /**
     * Returns `true` if this set contains
     * the specified element.
     */
    fun contains(element: E): Boolean {
        var prev = head
        var cur : Node<E>? = null
        var next : Node<E>?
        for (i in MAX_LEVEL-1 downTo 0) {
            cur = prev.next[i].reference

            // find cur >= element
            while (cur != tail) {
                val marked = BooleanArray(1)
                next = cur!!.next[i].get(marked)
                while (marked[0]) {
                    cur = prev.next[i].reference
                    next = cur.next[i].get(marked)
                }

                if (cur != tail && cur!!.element!! < element) {
                    prev = cur
                    cur = next
                } else {
                    break
                }
            }
        }
        return cur != tail && cur!!.element == element
    }
}

class Node<E>(val element: E?, val levels: Int = ThreadLocalRandom.current().nextInt(MAX_LEVEL) + 1) {
    val next = Array<AtomicMarkableReference<Node<E>>>(levels) {AtomicMarkableReference(null, false)}
}

class Result<E> {
    val prev = arrayOfNulls<Node<E>>(MAX_LEVEL)
    val next = arrayOfNulls<Node<E>>(MAX_LEVEL)
    var found = false
}

private const val MAX_LEVEL = 3

