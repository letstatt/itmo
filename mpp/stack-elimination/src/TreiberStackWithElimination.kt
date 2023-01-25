package mpp.stackWithElimination

import kotlinx.atomicfu.atomic
import kotlinx.atomicfu.atomicArrayOfNulls
import java.util.concurrent.ThreadLocalRandom

class TreiberStackWithElimination<E>() {
    private val top = atomic<Node<E>?>(null)
    private val eliminationArray = atomicArrayOfNulls<Operation<E>>(ELIMINATION_ARRAY_SIZE)

    private enum class State {
        FREE, WAITING, FINISHED
    }

    private class Operation<E>(val state: State, val node: E?)
    private val noop = Operation<E>(State.FREE, null)
    private val done = Operation<E>(State.FINISHED, null)

    init {
        for (i in (0 until ELIMINATION_ARRAY_SIZE)) {
            eliminationArray[i].value = noop
        }
    }

    /**
     * Adds the specified element [x] to the stack.
     */
    fun push(x: E) {
        // use elimination array
        val ind = ThreadLocalRandom.current().nextInt(ELIMINATION_ARRAY_SIZE)
        val op = Operation(State.WAITING, x)
        val prev = eliminationArray[ind].value

        if (prev!!.state == State.FREE) {
            // find a random place in an elimination array
            if (eliminationArray[ind].compareAndSet(prev, op)) {

                // if succeeded, wait for it's done
                for (i in (1..10)) {
                    val cell = eliminationArray[ind].value
                    if (cell!!.state == State.FINISHED) {
                        if (eliminationArray[ind].compareAndSet(cell, noop)) {
                            return
                        }
                    }
                }

                // revert manually, but it could be surprisingly done
                if (eliminationArray[ind].getAndSet(noop)!!.state == State.FINISHED) {
                    return
                }
            }
        }

        while (true) {
            // access top
            val head = top.value
            val node = Node(x, head)
            if (top.compareAndSet(head, node)) {
                return
            }
        }
    }

    /**
     * Retrieves the first element from the stack
     * and returns it; returns `null` if the stack
     * is empty.
     */
    fun pop(): E? {
        // look inside an array
        val ind = ThreadLocalRandom.current().nextInt(ELIMINATION_ARRAY_SIZE)
        val op = eliminationArray[ind].value

        // found its pair
        if (op!!.state == State.WAITING) {
            if (eliminationArray[ind].compareAndSet(op, done)) {
                return op.node
            }
        }

        while (true) {
            // access top
            val head = top.value ?: return null
            if (top.compareAndSet(head, head.next)) {
                return head.x
            }
        }
    }
}

private class Node<E>(val x: E, val next: Node<E>?)

private const val ELIMINATION_ARRAY_SIZE = 2 // DO NOT CHANGE IT