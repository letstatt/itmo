package mpp.linkedlistset

import java.util.concurrent.atomic.AtomicMarkableReference

class LinkedListSet<E : Comparable<E>> {
    private val first = Node<E>(element = null, next = null)
    private val last = Node<E>(element = null, next = null)
    init {
        first.casNext(null, last, false, false)
    }

    private fun finder(element: E): Pair<Node<E>, Node<E>> {
        while (true) {
            var cur = first
            while (true) {
                val next = cur.next!!
                val markableRef = next._next

                if (markableRef.isMarked) { // helping
                    cur.casNext(next, markableRef.reference, false, false)
                    break // try again
                }

                if (next == last) {
                    return Pair(cur, next) // reached end
                } else if (next.element == element) {
                    return Pair(cur, next) // OK
                } else if (next.element > element) {
                    return Pair(cur, next) // no such elem
                } else {
                    cur = next // go on
                }
            }
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
        while (true) {
            val (cur, next) = finder(element)
            if (next == last || next.element > element) {
                val node = Node(element, next)
                if (cur.casNext(next, node, false, false)) {
                    return true
                }
            } else {
                if (!next.isRemoved) {
                    return false // already exists
                }
                // try again
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
            val (cur, next) = finder(element)
            if (next == last || next.element > element) {
                return false
            } else {
                val nextNext = next.next
                if (next._next.attemptMark(nextNext, true)) { // mark as removed
                    if (cur.casNext(next, nextNext, false, false)) { // attempt to remove
                        return true
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
        val node = finder(element).second
        if (node._element == null || node.element != element || node.isRemoved) {
            return false // no such element
        }
        return true
    }
}

private class Node<E : Comparable<E>>(element: E?, next: Node<E>?) {
    val _element = element
    val element get() = _element!!

    val _next = AtomicMarkableReference(next, false)
    val next get() = _next.reference
    val isRemoved get() = _next.isMarked
    fun casNext(expected: Node<E>?, update: Node<E>?, old: Boolean, new: Boolean) =
        _next.compareAndSet(expected, update, old, new)
}